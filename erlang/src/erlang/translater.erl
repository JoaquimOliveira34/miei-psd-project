
-module(translater).

-include("../protos.hrl"). 


%% API
-export( [decode_Authentication/1, decode_MsgInvestor/1, decode_MsgCompany/1,
          encode_Reply/1, encode_Reply/2,encode_MsgExchange/2,
          setIdMsgInvestor/2, setIdMsgCompany/2, getCompanyMsgInvestor/1] ).


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

%%%%%%%%%% DECODE %%%%%%%%%%

%return record   
decode_MsgInvestor( Bin ) ->
    proto:decode_msg( Bin, 'MsgInvestor' ).

%return record
decode_MsgCompany( Bin ) ->
    proto:decode_msg( Bin, 'MsgCompany' ).
    

%return {Type, User, Name, Pass} 
decode_Authentication( Bin ) ->
    Msg = protos:decode_msg( Bin, 'Authentication'),
    list_to_tuple( records:record_info( fields,  Msg) ).

%%%%%%%%%% ENCODE %%%%%%%%%%

%return Bin
encode_Reply( Bool ) ->
    protos:encode_msg( #'ServerResponse'{ response = Bool } ).

%return Bin
encode_Reply( Bool, Message ) ->
    protos:encode_msg( #'ServerResponse'{ response = Bool, error = Message } ).

%return Bin
encode_MsgExchange( investor, MsgInvestor) ->
     Msg = #'MsgExchange'{ type = 'INVESTOR', investor = MsgInvestor },
     protos:encode_msg( Msg);

encode_MsgExchange( company, MsgCompany) ->
     Msg = #'MsgExchange'{ type = 'COMPANY', company = MsgCompany },
     protos:encode_msg( Msg).

    
    