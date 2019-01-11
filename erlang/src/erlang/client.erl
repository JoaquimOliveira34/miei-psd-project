
-module(client).

-include("../erlzmq.hrl"). 


%% API
-export([init/1]).


init( Port ) ->
      
    {ok, LSock} = gen_tcp:listen( Port, [binary, {packet, line}, {reuseaddr, true}]),
    
    acceptor(LSock).


acceptor( LSock ) ->

    {ok, Sock} = gen_tcp:accept( LSock ),
    
    spawn( fun() -> acceptor( LSock ) end),
        
    client( Sock ).


client( Sock ) ->
    receive 
        { tcp, _ , Bin } ->
            case makeLogin( Bin) of 
                {Reply, Id, Type} ->
                    gen_tcp:send(Sock, Reply),
                    exchanges:login(Id),
                    client( Sock, Id, Type);
    
                 Reply -> 
                     gen_tcp:send(Sock, Reply),
                     client( Sock )
            end;
                    
        { tcp_closed, _ }  -> ok ;
    
        { tcp_error, _, _} -> ok
    
    end.

    

%% Authenticated client to do send messages to exchanges 
client( Sock, Id, company ) ->
    receive 
        { tcp, _ , Bin } ->
            Received = translater:decode_MsgCompany( Bin ),
            WithId = translater:setIdMsgCompany( Received, Id),
            BinToExchange= translater:encode_MsgExchange( company, WithId),
            exchanges:send( BinToExchange, Id ),
            client( Sock, Id, company);

         {reply, Bin} ->
            gen_tcp:send( Bin ),
            client( Sock, Id, company);             
    
        { tcp_closed, _ }  -> 
            exchanges:logout(Id);
    
        { tcp_error, _, _} -> 
            exchanges:logout(Id)
    end;

client( Sock, Id, investor ) -> 
    receive 
        { tcp, _ , Bin } ->
            Received = translater:decode_MsgInvestor( Bin ),
            WithId = translater:setIdMsgInvestor( Received, Id),
            TargetCompanyId = translater:getCompanyMsgInvestor( WithId),
            BinToExchange= translater:encode_MsgExchange( investor, WithId),
            exchanges:send( BinToExchange, TargetCompanyId ),
            client( Sock, Id, investor);

         {reply, Bin} ->
            gen_tcp:send( Bin ),
            client( Sock, Id, investor);             
    
        { tcp_closed, _ }  -> 
            exchanges:logout(Id);
    
        { tcp_error, _, _} -> 
            exchanges:logout(Id)
    end.


%%  Authentication process 
%% @param Bin  binary message with Authentication info
%% @return  :: Binary | { Binnary, Id, Type } if the user is authenticated 
makeLogin( Bin ) -> 
    
    {Type, User, Name, Pass} = translater:decode_Authentication( Bin ),

    
    case Type of    
        
        'REGISTER' -> Result = accounts:create_account( Name, Pass, User);
        
        'LOGIN' -> Result = accounts:verify( Name, Pass)
    end,

    case Result of 
    
        ok -> 
            translater:encode_Reply( result, "Conta criada com sucesso");
            
        error -> 
            translater:encode_Reply( error, "Credenciais invalidas.");

        {ok, Id, Type} ->
            { translater:encode_Reply(result, "Login valido. Bem vindo"), Id, Type }
    end.

    
    