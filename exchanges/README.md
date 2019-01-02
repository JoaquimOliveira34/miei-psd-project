# Exchanges

## Installing Dependencies
If `wget` is installed, one can run simply:
```shell
make install
```
Otherwise download the following libraries manually to the `lib/` folder:
 - JeroMQ v0.4.3 [https://search.maven.org/remotecontent?filepath=org/zeromq/jeromq/0.4.3/jeromq-0.4.3.jar]
 - Protobuf v3.6.1 [https://search.maven.org/remotecontent?filepath=com/google/protobuf/protobuf-java/3.6.1/protobuf-java-3.6.1.jar]
 - OkIO v2.1.0 [https://search.maven.org/remotecontent?filepath=com/squareup/okio/okio/2.1.0/okio-2.1.0.jar]
 - OkHttp v3.12.1 [https://search.maven.org/remotecontent?filepath=com/squareup/okhttp3/okhttp/3.12.1/okhttp-3.12.1.jar]
 - Jackson Core v2.9.8 [https://search.maven.org/remotecontent?filepath=com/fasterxml/jackson/core/jackson-core/2.9.8/jackson-core-2.9.8.jar]
 - Jackson Annotations v2.9.8 [[https://search.maven.org/remotecontent?filepath=com/fasterxml/jackson/core/jackson-annotations/2.9.8/jackson-annotations-2.9.8.jar]
 - Jackson Databind v2.9.8 [https://search.maven.org/remotecontent?filepath=com/fasterxml/jackson/core/jackson-databind/2.9.8/jackson-databind-2.9.8.jar]
 - Kotlin StdLib v1.3.11 [https://search.maven.org/remotecontent?filepath=org/jetbrains/kotlin/kotlin-stdlib/1.3.11/kotlin-stdlib-1.3.11.jar] (needed by OkHttp)

## Compile
Run

```shell
make compile
make run
```
