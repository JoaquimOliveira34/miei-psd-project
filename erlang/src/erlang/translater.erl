
-module(translater).

-include("../protos.hrl"). 


%% API
-export( [] ).


% @param Bin  - Mensagem binaria do tipo 'MsgInvestor'
% @return  campo company da mensagem 
getCompany( Bin) ->

    Msg = proto:decode_msg( Bin, 'MsgInvestor' ),
    Msg#'MsgInvestor'.company.



% @param Id  - id do cliente 
% @param Bin  - Mensagem binaria do tipo 'MsgInvestor'
% @return  A mensagem em binario com o valor investorId preenchido com o valor dado 
addClientId( investor, Id, Bin ) -> 

    Msg = protos:decode_msg( Bin, 'MsgInvestor'),
    protos:encode( Msg#'MsgInvestor'{ investorId = Id });


% @param Id  - id da company
% @param Bin  - Mensagem binaria do tipo 'MsgCompany'
% @return  A mensagem em binario com o valor companyId  preenchido com o valor dado 
addClientId( company, Id, Bin ) ->

    Msg = protos:decode_msg( Bin, 'MsgCompany'),
    protos:encode( Msg#'MsgCompany'{ companyId = Id } ).
  

getAuthenticationData( Bin )->

    Msg = protos:decode_msg( Bin, 'Authentication'),
    list_to_tuple( records:record_info( fields,  Msg) ).


newResponse( Bool ) ->
    
    protos:encode_msg( #'ServerResponse'{ response = Bool } ).
