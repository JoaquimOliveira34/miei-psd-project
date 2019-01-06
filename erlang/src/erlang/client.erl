
-module(client).

-include("../erlzmq.hrl"). 


%% API
-export([init/1]).


init( Context ) ->
    
    {ok, XrepSocket } = erlzmq:socket(Context, xrep),

    ok = erlzmq:bind( XrepSocket, "tcp://*:12345" ),

    receiver( XrepSocket, #{} ).


%% Map  Addres => Client (pid)
receiver( XrepSocket, Map ) ->
    
    {Addr, Bottom, Body} = receiveMessage( XrepSocket ),

    case maps:get( Addr, Map, none ) of

        none ->    
            Pid = spawn( fun() -> client( XrepSocket, Addr ) end ),
            Pid ! {msg, Bottom, Body},
            receiver( XrepSocket, maps:put( Addr, Pid, Map) );        
        
        Pid ->
            Pid ! {msg, Bottom, Body},
            receiver( XrepSocket, Map)
    
    end.
    

%% Client to do Authentication process
client( XrepSocket, MyAddr ) -> 
    receive 
        {msg, Bottom, Bin} ->
            
            case  makeLogin( translater:decode_Authentication( Bin ) ) of 
                
                ok -> 
                    sendMessage( XrepSocket, MyAddr,Bottom, translater:encode_Reply(true) ),
                    client( XrepSocket, MyAddr);
                
                error -> 
                    sendMessage( XrepSocket, MyAddr,Bottom, translater:encode_Reply(false) ),
                    client( XrepSocket, MyAddr);
                
                {ok, Id, Type} ->
                    sendMessage( XrepSocket, MyAddr,Bottom, translater:encode_Reply(true, Type) ),
                    client( XrepSocket, MyAddr, Id, Type)
            end
    end.


%% Authenticated client to do send messages to exchanges 
client( XrepSocket, MyAddr, MyId, company ) ->
    receive 
        { msg,Bottom, Bin} ->
            Msg = translater:decode_MsgCompany( Bin ),
            Msg2 = translater:setIdMsgCompany( Msg, MyId),
            Bin2= translater:encode_MsgExchange( company, Msg2),
            exchanges:send( Bin2,Bottom, MyId );
            
        {reply, Bottom, Bin} -> 
            sendMessage( XrepSocket, MyAddr, Bottom, Bin)
    end,
    client( XrepSocket, MyAddr, MyId, company);

client( XrepSocket, MyAddr, MyId, investor ) -> 
    receive 
        { msg, Bin} ->
            Msg = translater:decode_MsgInvestor( Bin ),
            Msg2 = translater:setIdMsgInvestor( Msg, MyId),
            CompanyId = translater:getCompanyMsgInvestor( Msg2),
            Bin2= translater:encode_MsgExchange( investor, Msg2),
            exchanges:send( Bin2, CompanyId );

        {reply, Bottom, Bin} -> 
            sendMessage( XrepSocket, MyAddr,Bottom, Bin)
    end,
    client( XrepSocket, MyAddr, MyId, investor).

%% return error | ok | {ok, Id, Type}
makeLogin( {Type, User, Name, Pass} ) ->

    case Type of    

        'REGISTER' ->
            accounts:create_account( Name, Pass, User);
        'LOGIN' ->
            accounts:verify( Name, Pass)
    end.
            

%% return ok                  
sendMessage(Socket, Addr, Bottom, Bin  ) ->
    ok = erlzmq:send( Socket, Addr, [sndmore]),
    ok = erlzmq:send( Socket, Bottom, [sndmore]),
    ok = erlzmq:send( Socket, Bin).

%% return { Addr, Body}
receiveMessage( Socket ) ->
     
    {ok, Addr} = erlzmq:recv( Socket ),
    {ok, Bottom} = erlzmq:recv( Socket ),
    {ok, Body} = erlzmq:recv( Socket ),
    { Addr, Bottom, Body}.

