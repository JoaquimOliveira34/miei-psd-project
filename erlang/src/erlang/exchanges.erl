
-module(exchanges).

%% API
-export( [init/2, send/2, login/1, logout/1 ]).


%% @param Ports :: [String] Lista de portas das exchanges
%% @param PullPort :: String Porta onde o erlang ira fazer bind do socket zmq Pull 
init( Ports, PullPort) ->

    {ok, Context} = erlzmq:context(),
    
    Pids = createExchanges( Ports, Context ),
    
    register( exchangesPids, spawn( fun() -> exchangesPids( Pids ) end) ),

    {ok, PullSocket } = erlzmq:socket( Context, pull),

    ok = erlzmq:bind( PullSocket, "tcp://*:" ++ PullPort ), 

    register( receiver,  spawn( fun() -> receiver( PullSocket, #{}) end) ).


%% @param PullSocket Socket por onde vamos receber mensagens
%% @param Map  Id -> Pid A cada Id de utilizador associa o Pid do cliente 
receiver( PullSocket, Map ) -> 
    receive
        {login, Id, Pid } ->
            receiver( PullSocket, maps:put(Id, Pid, Map));
        
        {logout, Id} ->
            receiver( PullSocket, maps:remove(Id, Map))
        
        after 0 ->
            Bin = zmq:recv( PullSocket),
            Id = translater:getIdServerResponse( translater:decode_ServerResponse(Bin) ),
            case maps:get( Id, Map, error) of 
                error -> 
                    ok;   %% podiamos guardar as mensagens para reenviar quando ele voltasse a fazer login 
                Pid -> 
                    Pid ! {reply, Bin}
            end
    end.


exchangesPids( Exchanges ) ->

  receive

    { send, Bin, Company } ->
    
        Pid = getExchange( Exchanges, Company),

        Pid ! {send, Bin},

        exchangesPids( Exchanges )

    end.


%% @param Socket - Socket por onde é feita a comunicação
exchange( PushSocket ) ->

    receive
        { send, Bin } ->
            ok = erlzmq:send( PushSocket, Bin)                    
    end.




createExchanges( Pids, Context ) -> createExchanges( Pids, Context, [] ).

createExchanges( [],  _ , Pids) -> Pids;

createExchanges( [ Port | Tail ], Context, Pids ) ->
    
    {ok, PushSocket } = erlzmq:socket( Context, push),

    ok = erlzmq:coonect( PushSocket, "tcp://localhost:" ++ Port ),

    Pid = spawn( fun() -> exchange( PushSocket ) end),

    createExchanges( Tail , [ Pid | Pids] ).

%% return Pid 
getExchange( List , Company ) ->

    Size = lists:size( List ),
    
    Index = Company rem Size,
  
    lists:nth( Index, List ).


%% Return void
send( Bin, Company ) -> 
    exchangesPids ! { send, Bin, Company }.

%% Return void
login( Id )->
    receiver ! {login, Id, self() }.

%% Return void
logout( Id) ->
    receiver ! {logout, Id}.