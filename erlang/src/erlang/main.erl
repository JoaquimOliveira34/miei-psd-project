
-module(main).


%% API
-export([main/0]).

main() ->

    accounts:init(),

    client:init( "12345" ),

    exchanges:init( ["11101","11102"]),
    
    ok.