
-module(exchanges).

%% API
-export( [init/2, send/2 ]).


init( Ports, Context) ->

    Pids = createExchanges( Ports, Context ),

    Room = spawn( fun() -> room( Pids ) end),
    
    register( room, Room ).


room( Exchanges ) ->

  receive

    { send, Company, Bin, From } ->
    
        Pid = getExchange( Exchanges, Company),
  
        Pid ! {send, Bin , From},

        room( Exchanges )

    end.


%% @param Socket - Socket por onde é feita a comunicação
exchange( ReqSocket ) ->

    receive
        {send, Bin, From } ->
            
            ok = erlzmq:send( ReqSocket, Bin),
            
            Reply = erlzmq:recv( ReqSocket ),
            
            From ! { reply, Reply }
    end.





createExchanges( Pids, Context ) -> createExchanges( Pids, Context, []).

createExchanges( [], _ , Pids) -> Pids;

createExchanges( [ Port | Tail ], Context, Pids ) ->
    
    {ok, ReQSocket } = erlzmq:socket(Context, req),

    ok = erlzmq:coonect( ReQSocket, "tcp://localhost:" ++ Port ),

    Pid = spawn( fun() -> exchange( ReQSocket ) end),

    createExchanges( Tail , [ Pid | Pids] ).

%% return Pid 
getExchange( List , Company ) ->

    Size = lists:size( List),
    
    Index = Company rem Size,
  
    lists:nth( Index, List ).

%% Return void
send( Bin, Company ) -> 
    room ! { Company, Bin, self() }.