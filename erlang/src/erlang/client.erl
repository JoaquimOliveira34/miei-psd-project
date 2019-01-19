
-module(client).

-include("../erlzmq.hrl"). 


%% API
-export([init/1]).


init( Port ) ->
      
    {ok, LSock} = gen_tcp:listen( Port, [binary, {active, true}, {reuseaddr, true}]),
    
    acceptor(LSock).


acceptor( LSock ) ->

    {ok, Sock} = gen_tcp:accept( LSock ),

    spawn( fun() ->acceptor( LSock ) end),
    
    io:format("New client\n"),
    
    client( Sock ).        


client( Sock ) ->
    receive 
        { tcp, _ , Bin } ->
            io:format("Received msg\n"),
            case makeLogin( Bin) of 
                {Reply, Id, Type} ->
                    tcpSend( Sock, Reply),
                    exchanges:login(Id),
                    client( Sock, Id, Type);
    
                 Reply -> 
                    tcpSend( Sock, Reply),
                    client( Sock )
            end;
                    
        { tcp_closed, _ }  -> io:format("Closed\n"), ok ;
    
        { tcp_error, _, _} -> io:format("Closed\n"), ok
    
    end.

    

%% Authenticated client to do send messages to exchanges 
client( Sock, Id, company ) ->
    io:format("Login completed"),
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
    
    {CredType, UserTypeStr, Name, Pass} = translater:decode_Authentication( Bin ),

    UserType = case UserTypeStr of
        'COMPANY' -> company;
        'INVESTOR' -> investor;
        _ -> invalid_type
    end,
    
    case CredType of    
        
        'REGISTER' -> Result = accounts:create_account( Name, Pass, UserType);
        
        'LOGIN' -> Result = accounts:verify( Name, Pass, UserType)
    end,

    case Result of 
    
        {ok, Id } ->
            { translater:encode_Reply( response, "Login valido. Bem vindo"), Id, UserType };

        ok -> 
            translater:encode_Reply( response, "Conta criada com sucesso");
            
        error -> 
            translater:encode_Reply( error, "Credenciais invalidas.")

    end.

    
tcpSend( Sock, Bin ) -> 
    Size = length( binary_to_list(Bin) ),
    SizeMsg = translater:encode_IntMessage(Size),
    io:format("Send [~w]+ ~w bytes\n",  [length( binary_to_list((SizeMsg))),Size]),
    gen_tcp:send( Sock, SizeMsg),
    gen_tcp:send( Sock, Bin).
    
