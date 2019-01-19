
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

    register( userPids, spawn( fun() -> userPids( #{} ) end) ),

    register( receiver,  spawn( fun() -> receiver( PullSocket ) end) ),
    io:format("Exchanges running...").


%% @param PullSocket Socket por onde vamos receber mensagens
%% @param Map  Id -> Pid A cada Id de utilizador associa o Pid do cliente 
receiver( PullSocket ) -> 
    {ok, Bin} = erlzmq:recv( PullSocket),
    Id = translater:getIdServerResponse( translater:decode_ServerResponse(Bin) ),
    case getUserPid( Id ) of 
        error -> 
            ok;   %% podiamos guardar as mensagens para reenviar quando ele voltasse a fazer login 
        Pid -> 
            Pid ! {reply, Bin}
    end,

    receiver( PullSocket ).


userPids ( Users ) ->
    receive
        {login, Id, Pid } ->
            userPids( maps:put(Id, Pid, Users));
            
        {logout, Id} ->
            userPids( maps:remove(Id, Users));
        {get, Id, Pid} ->
            case maps:get( Id, Users, error) of 
                error -> 
                    Pid ! { getUserPid, error };
                UserPid -> 
                    Pid ! { getUserPid, UserPid }
            end,
            userPids( Users )
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
    end,

    exchange( PushSocket ).




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

    Size = length( List ),
    
    Index = Company rem Size,
  
    lists:nth( Index, List ).

%% return Pid | error
getUserPid ( Id ) ->
    receiver ! { get, Id, self() },

    receive
        { getUserPid, error } -> error;
        { getUserPid, UserPid } -> UserPid
    end.


%% Return void
send( Bin, Company ) -> 
    exchangesPids ! { send, Bin, Company },
    ok.

%% Return void
login( Id )->
    userPids ! { login, Id, self() },
    ok.

%% Return void
logout( Id) ->
    userPids ! {logout, Id},
    ok.
