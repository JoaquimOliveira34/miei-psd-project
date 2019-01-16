
-module(main).


%% API
-export([main/0]).

main() ->

    accounts:init(),

    exchanges:init( ["11101","11102"], "9999"),
    
    client:init( 12345 ),

    
    receive
        block -> ok
    end.
