
-module(exchanges).

%% API
-export( [init/1, send/2 ]).


%% @param Ports - List of Exchanges ports
init( Ports ) ->

    Pids = createExchanges( Ports, [] ),

    spawn( fun() -> room( Pids ) end).



%% param Exchanges - List of exchanges ( Pids )
room( Exchanges ) ->

  receive

    { send, Company, Data, From } ->
    
        Pid = getExchange( Exchanges, Company),
        Pid ! {data, Data, From},
        room( Exchanges )

  end.



%% @param Socket - Socket por onde é feita a comunicação
%% @param Map - Mapa com o Id -> Pid para responder diretamente ao cliente
exchange( Socket ) ->

    receive
        {send, Data, From } ->
            gen_tcp:send(Socket, Data),
            exchange( Socket  );

        {tcp, _ , Data} ->
            exchange( Socket );

        {tcp_closed, _} ->
          error;
        {tcp_error, _, _} ->
          error
    end.



%% Auxiliary functions


createExchanges( [], Pids) -> Pids;

createExchanges( [ Port | Tail ], Pids ) ->

    {ok, Socket} = gen_tcp:connect( {127,0,0,1}, Port, [binary, {active,true}]),

    Pid = spawn( fun() -> exchange( Socket ) end),

    createExchanges( Tail , [ Pid | Pids] ).


getExchange( List , Company ) ->
  Size = lists:size( List),
  Hash = erlang:phash2( Company, Size),
  lists:nth( Hash, List ).

send( company , Data )  -> true;

send( client , Data )  -> false.