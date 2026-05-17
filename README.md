## Simple Demo to showcase vilcacora's functionalities

This demo runs a simple HTTP inference server using a pre-trained MNIST model.
It showcases model loading, session pool management, and serving inference requests over HTTP.

## Prerequisites

- [Scala Native toolchain](https://scala-native.org/en/stable/user/setup.html)
- An ONNX MNIST model placed at `src/main/resources/mnist12_static.onnx`

## Running

```bash
sbt run
```

The server starts on `http://0.0.0.0:8080`.

## Endpoints

**`GET /hello/:name`**
```bash
curl http://localhost:8080/hello/world
# Hello, world.
```

**`POST /infer`**

Accepts a flat JSON array of `784` floats (1×1×28×28) representing a grayscale MNIST image, returns a JSON array of `10` floats (class probabilities).

```bash
curl -X POST http://localhost:8080/infer \
  -H "Content-Type: application/json" \
  -d '[0.0, 0.0, ..., 0.0]'  # 784 floats
```

## What it demonstrates

- **`ModelLoader`** — loading an ONNX model from the classpath
- **`InterpreterUtils.inferenceSessionPool`** — managing a pool of concurrent inference sessions with correct native memory lifecycle
- **`InterpreterUtils.copyArrayToBuffer`** — safely copying request data into pre-allocated session buffers
- **Session reuse** — input buffers are pre-allocated per session; each request copies into them rather than allocating on every call
