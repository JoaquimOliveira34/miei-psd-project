
-module(exchanges).

-include("../erlzmq.hrl"). 

%% API
-export( [init/2, send/2, login/1, logout/1 ]).


%% @param Ports :: [String] Lista de portas das exchanges
%% @param PullPort :: String Porta onde o erlang ira fazer bind do socket zmq Pull 
init( Ports, PullPort) ->

    {ok, Context} = erlzmq:context(),
    {ok, PullSocket } = erlzmq:socket( Context, [pull, {active, false}]),

    {ok, PP } = erlzmq:socket( Context, [push, {active, false}]),

    Address = string:concat( "tcp://*:" , PullPort),
    ok = erlzmq:bind( PullSocket,  Address), 

    Pids = createExchanges( Ports, Context ),
    register( exchangesPids, spawn( fun() -> exchangesPids( Pids ) end) ),
    
    register( receiver,  spawn( fun() -> receiver( PullSocket, #{} ) end) ),
    io:format("Exchanges running...").


%% @param PullSocket Socket por onde vamos receber mensagens
%% @param Map  Id -> Pid A cada Id de utilizador associa o Pid do cliente 
receiver( PullSocket, Map ) -> 
    receive
        {login, Id, Pid } ->
            receiver( PullSocket, maps:put(Id, Pid, Map));
        
        {logout, Id} ->
            receiver( PullSocket, maps:remove(Id, Map))
        
        after 0 ->
            Bin = erlzmq:recv( PullSocket),
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




createExchanges( Ports, Context ) -> createExchanges( Ports, Context, [] ).

createExchanges( [],  _ , Pids) -> Pids;

createExchanges( [ Port | Tail ], Context, Pids ) ->
    
    {ok, PushSocket } = erlzmq:socket( Context, [push, {active, false}]),

    Address = "tcp://localhost:" ++ Port,

    ok = erlzmq:connect( PushSocket, Address ),

    Pid = spawn( fun() -> exchange( PushSocket ) end),

    createExchanges( Tail , Context, [ Pid | Pids] ).

%% return Pid 
getExchange( List , Company ) ->

    Size = lists:size( List ),
    
    Index = Company rem Size,
  
    lists:nth( Index, List ).


%% Return void
send( Bin, Company ) -> 
    exchangesPids ! { send, Bin, Company },
    ok.

%% Return void
login( Id )->
    receiver ! { login, Id, self() },
    ok.

%% Return void
logout( Id) ->
    receiver ! {logout, Id},
    ok.