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

  // Case class to encapsulate the complete inference session
  case class InferenceSession(
      inputBuffer: Array[Float],
      runInference: IO[Map[String, Any]],
  )

  def makeInputBuffer(): Array[Float] =
    Array.fill(1 * 1 * 28 * 28)(0f)

  // A resource that manages the complete inference session
  def inferenceSessionResource(
      modelIR: ModelIR,
      inputName: String,
  ): Resource[IO, InferenceSession] =
    for {
      inputBuffer <- Resource.eval(IO(makeInputBuffer()))
      runInference <- Interpreter.execute(modelIR, Map(inputName -> inputBuffer))
    } yield InferenceSession(inputBuffer, runInference)

  // KeyPool for managing inference sessions
  def inferenceSessionPool(
      modelIR: ModelIR,
      inputName: String,
  ): Resource[IO, KeyPool[IO, Unit, InferenceSession]] =
    KeyPool
      .Builder[IO, Unit, InferenceSession](
        // Create function - creates a new inference session with allocated memory
        create = _ => inferenceSessionResource(modelIR, inputName).allocated.map(_._1),
        // Destroy function - since we're using Resource.allocated, the cleanup is handled automatically
        // We can make this a no-op since the Resource cleanup will be called when the pool is destroyed
        destroy = _ => IO.unit,
      )
      .withMaxPerKey(_ => 2) //  inference session per key (we're using Unit as key)
      .withMaxTotal(2) // Total of  inference session in the pool
      .withIdleTimeAllowedInPool(Duration.Inf) // Never destroy due to inactivity
      .build

  def run(args: List[String]): IO[ExitCode] = {
    val appSetup = for {
      _ <- IO.println("Loading Model")
      modelproto <- loadModelFromPath("/mnist12_static.onnx")
      modelIR <- translateModel(modelproto)
      _ <- IO.println("Successfully Loaded Model")

      inputName <- IO.fromOption(modelIR.graphInputs.headOption)(
        new RuntimeException("Model has no graph inputs"),
      )
      outputName <- IO.fromOption(modelIR.graphOutputs.headOption)(
        new RuntimeException("Model has no graph outputs"),
      )
    } yield (modelIR, inputName, outputName)

    appSetup
      .flatMap { case (modelIR, inputName, outputName) =>
        // Use the inference session pool
        inferenceSessionPool(modelIR, inputName)
          .use { pool =>
            IO.println("Starting HTTP service on 0.0.0.0:8080") >> {
              val inferApp = HttpRoutes
                .of[IO] {
                  case GET -> Root / "hello" / name =>
                    Ok(s"Hello, $name.")

                  case req @ POST -> Root / "infer" =>
                    // Take a lease on the complete inference session
                    pool.take(()).use { managedSession =>
                      val session = managedSession.value
                      (for {
                        inputArray <- req.as[Array[Float]]

                        _ <- IO {
                          if (inputArray.length != session.inputBuffer.length) {
                            throw new IllegalArgumentException(
                              s"Invalid input size: expected ${session.inputBuffer.length}, got ${inputArray.length}",
                            )
                          }
                          // This mutation is now safe from race conditions
                          System.arraycopy(
                            inputArray,
                            0,
                            session.inputBuffer,
                            0,
                            session.inputBuffer.length,
                          )
                        }
                        // Use the pre-allocated inference session
                        outputMap <- session.runInference
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
