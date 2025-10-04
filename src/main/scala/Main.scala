import cats.effect._
import com.comcast.ip4s._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.ember.server._
import scala.util.Success
import java.io.InputStream
import java.nio.{ByteBuffer, ByteOrder}
import vilcacora.onnx.proto.ModelProto
import vilcacora.onnx.Translator._
import com.armanbilge.vilcacora.ir._
import com.armanbilge.vilcacora.runtime._
import vilcacora.onnx.Translator
object Main extends IOApp {

  def loadModelFromPath(modelPath: String): IO[ModelProto] = IO {
    val stream: InputStream = getClass.getResourceAsStream(modelPath)
    if (stream == null)
      throw new IllegalArgumentException(s"Resource not found: $modelPath")
    try ModelProto.parseFrom(stream)

    finally stream.close()
  }

  val helloWorldService = HttpRoutes
    .of[IO] { case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
    }
    .orNotFound

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
  def run(args: List[String]): IO[ExitCode] =

    for {
      _ <- IO.println("Loading Model")
      modelproto <- loadModelFromPath("/mnist12_static.onnx")
      modelIR <- IO.fromEither(
        translate(modelproto).left.map(errStr => new RuntimeException(errStr)),
      )
      _ <- IO.println("Succesfully Loaded Model")
      _ <- printModel(modelIR) // for printing the mode
      _ <- IO.println("Starting HTTP service on 0.0.0.0:8080")
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(helloWorldService)
        .build
        .use(_ => IO.never)
    } yield ExitCode.Success
}
