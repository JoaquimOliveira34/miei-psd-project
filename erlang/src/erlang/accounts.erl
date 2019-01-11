
-module(accounts).

%% API
-export( [init/0, create_account/3, verify/2]).

init()->

    Users = rest:getUsers(),

    Pid = spawn( fun() -> accounts(  Users , length( maps:keys(Users) )) end),

    register( accounts , Pid).


%Users = #{ quim => {id, pass, investor}, pedro => {id, pass2,investor} , ...}
accounts( Map, NextId )->

    receive
        { verify , Username, Passwd, Pid } ->
            case maps:find( Username, Map) of
                {ok, { Id, Passwd, Type} } ->
                    Pid ! {ok, Id, Type} ;
                _ ->
                    Pid ! error
                
            end;

        { create, Username, Passwd, Type, Pid} ->
            
            case maps:is_key(Username, Map) of
                true ->
                    Pid ! error;
                false ->
                    Pid ! ok,
                    Map = (maps:put( Username ,{ NextId, Passwd, Type} , Map) ),
                    % enviar informaçao para o diretorio!!!
                    accounts(Map, NextId + 1)
            end
    end,
    accounts(Map, NextId).


% return  ok || error 
create_account( Username, Passwd, Type) ->
    accounts ! {create, Username, Passwd, Type, self()},
   
    receive
        V -> V
    end.


% return  {ok, Id, Type} || error 
verify( Username, Passwd) ->

    accounts ! { verify, Username, Passwd, self() },
    
    receive
        {ok, Id, Type} -> {ok, Id, Type };
        error -> error 
    end.
