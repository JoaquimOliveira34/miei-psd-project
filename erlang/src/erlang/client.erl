
-module(client).

-include("../erlzmq.hrl"). 


%% API
-export([init/0]).

init() ->

    {ok, Context} = erlzmq:context(),
    
    {ok, XrepSocket } = erlzmq:socket(Context, xrep),

    ok = erlzmq:bind( XrepSocket, "tcp://*:12345" ),

    receiver( XrepSocket, #{} ).


%% Map  Addres => Client (pid)
receiver( XrepSocket, Map ) ->
    
    {ok, Addr} = erlzmq:recv( XrepSocket ),

    {ok, Body} = erlzmq:recv(XrepSocket),

    case maps:get( Addr, Map, error ) of

        error ->    
            Pid = spawn( fun() -> client( XrepSocket, Addr ) end ),
            Pid ! {msg, Body},
            receiver( XrepSocket, maps:put( Addr, Pid, Map) );        
        
        Pid ->
            Pid ! {msg, Body},
            receiver( XrepSocket, Map)
    
    end.
    


client( XrepSocket, MyAddr ) -> 
    receive 
        {msg, Bin} ->
                        
            Res =  makeLogin( = translater:getAuthenticationData( Bin ) ),
            case Res of 
         
                error -> 
                    sendMessage( XrepSocket, MyAddr, translater:newResponse(false) ),
                    client( XrepSocket, MyAddr);
                
                {ok, Type} ->
                    sendMessage( XrepSocket, MyAddr, translater:newResponse(true) ),
                    client( XrepSocket, MyAddr, Type)
            end
    end.


client( XrepSocket, MyAddr, company ) -> true;
client( XrepSocket, MyAddr, investor ) -> false.


makeLogin( {Type, User, Name, Pass} ) ->

    case Type of    
        register ->
            accounts:create_account( Name, Pass, User);
        login ->
            accounts:verify( Name, Pass)
    end.
            
                    
sendMessage(Socket, Addr, Bin  ) ->
    ok = erlzmq:send( Socket, Addr, [sndmore]),
    ok = erlzmq:send( Socket, Bin).