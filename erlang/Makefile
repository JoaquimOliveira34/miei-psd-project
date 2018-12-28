ProtoFile = protos


compile: ./../$(ProtoFile).proto  src/erlzmq.hrl    deps/gpb deps/erlzmq2/ebin

#compile proto files	
	./deps/gpb/bin/protoc-erl -I. ./../$(ProtoFile).proto
	erlc -I ./deps/gpb/include  ./../$(ProtoFile).erl
	mv ./../$(ProtoFile).hrl src/
	
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
	rm  ./../$(ProtoFile).erl

remove:
	rm -f src/$(ProtoFile).*
	rm -f ./../$(ProtoFile).erl
	rm -f ./../$(ProtoFile).hrl
	rm -f target/*.beam
	rm -f *.beam
	