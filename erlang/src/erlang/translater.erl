-module(translater).

-include("../protos.hrl"). 


%% API
-export( [decode_Authentication/1, decode_MsgInvestor/1, decode_MsgCompany/1,decode_ServerResponse/1,
          encode_Reply/2,encode_MsgExchange/2,encode_IntMessage/1,
          setIdMsgInvestor/2, setIdMsgCompany/2, getCompanyMsgInvestor/1, getIdServerResponse/1] ).




%%%%%%%%%% RECORDS %%%%%%%%%%

% Return record
setIdMsgInvestor( Record, Value) ->
    Record#'MsgInvestor'{ investorId = Value }.

% Return record
setIdMsgCompany( Record, Value) ->
    Record#'MsgCompany'{ companyId = Value }.

% Return Company
getCompanyMsgInvestor( Record) -> 
    Record#'MsgInvestor'.company.

getIdServerResponse( Record ) ->
    Record#'ServerResponse'.userId.





%%%%%%%%%% DECODE %%%%%%%%%%
%return record
decode_ServerResponse( Bin) ->
    protos:decode_ServerResponse( Bin, 'ServerResponse').

%return record   
decode_MsgInvestor( Bin ) ->
    protos:decode_msg( Bin, 'MsgInvestor' ).

%return record
decode_MsgCompany( Bin ) ->
    protos:decode_msg( Bin, 'MsgCompany' ).
    

%return {CredType, UserType, Name, Pass} 
decode_Authentication( Bin ) ->
    Msg = protos:decode_msg( Bin, 'Authentication'),
    {   Msg#'Authentication'.credentialsType, 
        Msg#'Authentication'.userType, 
        Msg#'Authentication'.username, 
        Msg#'Authentication'.password
    }.





%%%%%%%%%% ENCODE %%%%%%%%%%

%return Bin
encode_Reply( error, Message ) ->
    protos:encode_msg( #'ServerResponse'{ userId = -1, error = Message } );
%return Bin
encode_Reply( response, Message ) ->
    protos:encode_msg( #'ServerResponse'{ userId = -1, response = Message } ).

%return Bin
encode_MsgExchange( investor, MsgInvestor) ->
     Msg = #'MsgExchange'{ type = 'INVESTOR', investor = MsgInvestor },
     protos:encode_msg( Msg);

%return Bin
encode_MsgExchange( company, MsgCompany) ->
     Msg = #'MsgExchange'{ type = 'COMPANY', company = MsgCompany },
     protos:encode_msg( Msg).

%return Bin
encode_IntMessage( Int)->
    Msg =  #'IntMessage'{ value = Int},
    protos:encode_msg( Msg).

    
    
