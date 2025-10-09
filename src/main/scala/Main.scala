import cats.effect._
import com.comcast.ip4s._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.ember.server._
import scala.util.Success

import vilcacora.onnx.proto.ModelProto
import vilcacora.onnx.Translator._
import com.armanbilge.vilcacora.ir._
import com.armanbilge.vilcacora.runtime._
import vilcacora.onnx.Translator
import io.circe.generic.auto._
import org.http4s.circe._

import fs2.io.readClassLoaderResource
import fs2.Stream
import org.http4s.Request
import org.http4s.EntityEncoder
import org.http4s.EntityDecoder
import org.http4s.Response
import org.typelevel.keypool.KeyPool
import scala.concurrent.duration._
object Main extends IOApp {

  def loadModelFromPath(modelPath: String): IO[ModelProto] = {
    val byteStream: Stream[IO, Byte] = readClassLoaderResource[IO](modelPath)
    byteStream.compile
      .to(Array)
      .flatMap(bytes => IO.blocking(ModelProto.parseFrom(bytes)))
      .handleErrorWith { error =>
        IO.println(s"Error loading model from $modelPath: ${error.getMessage}") >>
          IO.raiseError(new RuntimeException(s"Failed to load model: ${error.getMessage}", error))
      }
  }

  def translateModel(proto: ModelProto): IO[ModelIR] =
    IO.blocking {
      translate(proto).fold(err => throw new RuntimeException(err), identity)
    }

  implicit val encoder: EntityEncoder[IO, Array[Float]] = jsonEncoderOf[IO, Array[Float]]
  implicit val decoder: EntityDecoder[IO, Array[Float]] = jsonOf[IO, Array[Float]]

  def printModel(model: ModelIR): IO[Unit] = IO {
    println("Model ------")
    println(s"Name: ${model.name}")

    println("Operations:")
    model.operations.foreach(op => println(s"  $op"))

    println("Allocations:")
    model.allocations.foreach { case (key, alloc) =>
      println(s"  $key -> $alloc")
    }

    println("Graph Inputs:")
    model.graphInputs.foreach(input => println(s"  $input"))

    println("Graph Outputs:")
    model.graphOutputs.foreach(output => println(s"  $output"))
  }

  def makeInputBuffer(): Array[Float] =
    Array.fill(1 * 1 * 28 * 28)(0f)

  def bufferPool(inputBuffer: Array[Float]): Resource[IO, KeyPool[IO, Unit, Array[Float]]] =
    KeyPool
      .Builder[IO, Unit, Array[Float]](
        // The pool doesn't create a new buffer, it just returns the existing one.
        create = _ => IO.pure(inputBuffer),
        // The destroy action is a no-op, so our buffer is never deallocated.
        destroy = _ => IO.unit,
      )
      .withMaxPerKey(_ => 1)
      .withMaxTotal(1)
      .withIdleTimeAllowedInPool(Duration.Inf) // Never destroy the buffer due to inactivity.
      .build

  def run(args: List[String]): IO[ExitCode] = {
    val appSetup = for {
      _ <- IO.println("Loading Model")
      modelproto <- loadModelFromPath("/mnist12_static.onnx")
      modelIR <- translateModel(modelproto)
      _ <- IO.println("Successfully Loaded Model")

      inputName <- IO.fromOption(modelIR.graphInputs.headOption)(
        new RuntimeException("Model has no graph inputs"), // should not happen
      )
      outputName <- IO.fromOption(modelIR.graphOutputs.headOption)(
        new RuntimeException("Model has no graph outputs"), // should not happen
      )
    } yield (modelIR, inputName, outputName)

    appSetup
      .flatMap { case (modelIR, inputName, outputName) =>
        // The single, shared buffer instance that will be protected by the pool.
        val inputBuffer = makeInputBuffer()

        // Compose the KeyPool and Interpreter resources.
        val serverResources = for {
          pool <- bufferPool(inputBuffer)
          runInference <- Interpreter.execute(modelIR, Map(inputName -> inputBuffer))
        } yield (pool, runInference)

        // Use the composed resources to run the server.
        serverResources
          .use { case (pool, runInference) =>
            IO.println("Starting HTTP service on 0.0.0.0:8080") >> {
              val inferApp = HttpRoutes
                .of[IO] {
                  case GET -> Root / "hello" / name =>
                    Ok(s"Hello, $name.")

                  case req @ POST -> Root / "infer" =>
                    // For each request, take a "lease" on the buffer.
                    // This will block if the buffer is already in use, ensuring sequential access.
                    pool.take(()).use { managedBuffer =>
                      val lockedInputBuffer = managedBuffer.value
                      (for {
                        inputArray <- req.as[Array[Float]]

                        _ <- IO {
                          if (inputArray.length != lockedInputBuffer.length) {
                            throw new IllegalArgumentException(
                              s"Invalid input size: expected ${lockedInputBuffer.length}, got ${inputArray.length}",
                            )
                          }
                          // This mutation is now safe from race conditions.
                          System.arraycopy(
                            inputArray,
                            0,
                            lockedInputBuffer,
                            0,
                            lockedInputBuffer.length,
                          )
                        }
                        // `runInference` uses the buffer that we just safely mutated.
                        outputMap <- runInference
                        output <- IO.fromOption(outputMap.get(outputName))(
                          new RuntimeException(s"Output $outputName not found in inference results"),
                        )
                        result <- IO(output.asInstanceOf[Array[Float]])
                        resp <- Ok(result)
                      } yield resp).handleErrorWith {
                        case e: org.http4s.DecodeFailure =>
                          BadRequest(s"Invalid JSON input: ${e.getMessage()}")
                        case e: IllegalArgumentException =>
                          BadRequest(e.getMessage)
                        case e: ClassCastException =>
                          InternalServerError(s"Internal error: unexpected output type")
                        case e =>
                          IO.println(s"Inference error: ${e.getMessage}") >>
                            InternalServerError(s"Inference failed: ${e.getMessage}")
                      }
                    }
                }
                .orNotFound

              EmberServerBuilder
                .default[IO]
                .withHost(ipv4"0.0.0.0")
                .withPort(port"8080")
                .withHttpApp(inferApp)
                .build
                .use(_ => IO.never)
            }
          }
          .as(ExitCode.Success)
      }
      .handleErrorWith { error =>
        IO.println(s"Application failed: ${error.getMessage}") >>
          IO.pure(ExitCode.Error)
      }
  }
}
