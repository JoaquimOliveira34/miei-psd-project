
-module(accounts).

%% API
-export( [init/0, create_account/3, verify/3]).

init()->

    Users = rest:getUsers(),

    Pid = spawn( fun() -> accounts(  Users , length( maps:keys(Users) )) end),

    register( accounts , Pid).


%Users = #{ quim => {id, pass, investor}, pedro => {id, pass2,investor} , ...}
accounts( Map, NextId )->

    receive
        { verify , Username, Passwd, Type, Pid } ->
            case maps:find( Username, Map) of
                {ok, { Id, Passwd, Type} } ->
                    Pid ! {ok, Id } ;
                _ ->
                    Pid ! error
                
            end;

        { create, Username, Passwd, Type, Pid} ->
            
            case maps:is_key(Username, Map) of
                true ->
                    Pid ! error;
                false ->
                    Pid ! ok,
                    NewMap = (maps:put( Username ,{ NextId, Passwd, Type} , Map) ),
                    rest:registerUser( Username, Passwd, Type, NextId),
                    accounts(NewMap, NextId + 1)
            end
    end,
    accounts(Map, NextId).


% return  ok || error 
create_account( Username, Passwd, Type) ->
    accounts ! {create, Username, Passwd, Type, self()},
   
    receive
        V -> V
    end.


% return  {ok, Id } || error 
verify( Username, Passwd, Type) ->

    accounts ! { verify, Username, Passwd, Type, self() },
    
    receive
        {ok, Id, Type} -> {ok, Id};
        error -> error 
    end.
