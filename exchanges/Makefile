space = $(eval) $(eval)

LIB_FOLDER = ./lib

LIBS = $(subst $(space),:,$(wildcard $(LIB_FOLDER)/*.jar))
SRC = $(wildcard src/**/*.java)

install:
	mkdir -p $(LIB_FOLDER)
	wget https://search.maven.org/remotecontent?filepath=org/zeromq/jeromq/0.4.3/jeromq-0.4.3.jar -nc -q --show-progress -O $(LIB_FOLDER)/jeromq-0.4.3.jar ||:
	wget https://search.maven.org/remotecontent?filepath=com/google/protobuf/protobuf-java/3.6.1/protobuf-java-3.6.1.jar -nc -q --show-progress -O $(LIB_FOLDER)/protobuf-java-3.6.1.jar ||:
	wget https://search.maven.org/remotecontent?filepath=com/squareup/okio/okio/2.1.0/okio-2.1.0.jar -nc -q --show-progress -O $(LIB_FOLDER)/okio-2.1.0.jar ||:
	wget https://search.maven.org/remotecontent?filepath=com/squareup/okhttp3/okhttp/3.12.1/okhttp-3.12.1.jar -nc -q --show-progress -O $(LIB_FOLDER)/okhttp-3.12.1.jar ||:
	wget https://search.maven.org/remotecontent?filepath=com/fasterxml/jackson/core/jackson-core/2.9.8/jackson-core-2.9.8.jar -nc -q --show-progress -O $(LIB_FOLDER)/jackson-core-2.9.8.jar ||:
	wget https://search.maven.org/remotecontent?filepath=com/fasterxml/jackson/core/jackson-annotations/2.9.8/jackson-annotations-2.9.8.jar -nc -q --show-progress -O $(LIB_FOLDER)/jackson-annotations-2.9.8.jar ||:
	wget https://search.maven.org/remotecontent?filepath=com/fasterxml/jackson/core/jackson-databind/2.9.8/jackson-databind-2.9.8.jar -nc -q --show-progress -O $(LIB_FOLDER)/jackson-databind-2.9.8.jar ||:
	wget https://search.maven.org/remotecontent?filepath=org/jetbrains/kotlin/kotlin-stdlib/1.3.11/kotlin-stdlib-1.3.11.jar -nc -q --show-progress -O $(LIB_FOLDER)/kotlin-stdlib-1.3.11.jar ||:

clean:
	rm -r ./bin

compile: $(SRC)
	mkdir -p ./bin
	javac -d ./bin -cp $(LIBS) ./src/**/*.java

ARGS ?= 12345 12346 12347 localhost 8080

# USAGE
# make run ARGS="12345 12346 12347 localhost 8080"
run:
	java -cp "./bin/:$(LIBS)" Exchanges.Main $(ARGS)
