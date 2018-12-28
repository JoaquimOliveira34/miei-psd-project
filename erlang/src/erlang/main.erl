
-module(main).

-include("../erlzmq.hrl"). 

%% API
-export([main/0]).

main() ->

    {ok, Context} = erlzmq:context(),
    client:init( Context ),
    exchanges:init( ["11101","11102"], Context),
    accounts:init(),
    ok.   