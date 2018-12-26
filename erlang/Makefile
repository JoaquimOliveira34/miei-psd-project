ProtoFile = protos


compile: src/$(ProtoFile).proto  src/erlzmq.hrl    deps/gpb deps/erlzmq2/ebin

#compile proto files	
	./deps/gpb/bin/protoc-erl -I. src/$(ProtoFile).proto
	erlc -I ./deps/gpb/include  src/$(ProtoFile).erl
	
	
#compile erl files
	@echo ""
	#############################
	## >>> Erlang messages <<< ##
	#############################
	@echo ""
	erlc src/erlang/*.erl
	@echo ""
	#############################
	###### >>> Finnish <<< ######	
	#############################
	@echo ""
	
#move files to target 
	mv  *.beam  target/
	rm  src/$(ProtoFile).erl

remove:
	rm -f src/$(ProtoFile).hrl
	rm -f target/*.beam
	

run:
	cd target/
	erl < run