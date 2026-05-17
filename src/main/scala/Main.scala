import cats.effect._
import com.comcast.ip4s._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.ember.server._
import scala.util.Success

import vilcacora.onnx.proto.ModelProto
import com.armanbilge.vilcacora.ir.ModelIR
import com.armanbilge.vilcacora.runtime.{Interpreter, InterpreterUtils}
import vilcacora.onnx.{Translator, ModelLoader}
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

  def translateModel(proto: ModelProto): IO[ModelIR] =
    IO.blocking {
      Translator.translate(proto).fold(err => throw new RuntimeException(err), identity)
    }

  implicit val encoder: EntityEncoder[IO, Array[Float]] = jsonEncoderOf[IO, Array[Float]]
  implicit val decoder: EntityDecoder[IO, Array[Float]] = jsonOf[IO, Array[Float]]

  def run(args: List[String]): IO[ExitCode] = {
    val appSetup = for {
      _ <- IO.println("Loading Model")
      modelproto <- ModelLoader.loadModelFromPath("/mnist12_static.onnx")
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
        InterpreterUtils
          .inferenceSessionPool(modelIR = modelIR)
          .use { pool =>
            IO.println("Starting HTTP service on 0.0.0.0:8080") >> {
              val inferApp = HttpRoutes
                .of[IO] {
                  case GET -> Root / "hello" / name =>
                    Ok(s"Hello, $name.")

                  case req @ POST -> Root / "infer" =>
                    // Take a lease on the complete inference session
                    pool.take(()).use { managedSession =>
                      val session = managedSession.value.session
                      (for {
                        inputArray <- req.as[Array[Float]]
                        inputBuffer <- IO.fromOption(
                          session.inputs.get(inputName).map(_.asInstanceOf[Array[Float]]),
                        )(
                          new RuntimeException(s"Input '$inputName' not found in session"),
                        )
                        _ <- IO {
                          if (inputArray.length != inputBuffer.length) {
                            throw new IllegalArgumentException(
                              s"Invalid input size: expected ${inputBuffer.length}, got ${inputArray.length}",
                            )
                          }
                        }
                        _ <- InterpreterUtils.copyArrayToBuffer(
                          inputArray,
                          0,
                          inputBuffer,
                          0,
                          inputBuffer.length,
                        )

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
