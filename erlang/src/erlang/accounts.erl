
-module(accounts).

%% API
-export( [init/0, create_account/3, verify/2]).

init()->

    Pid = spawn( fun() -> accounts( #{ "quim"  => {"pass", investor },
                                       "pedro" => {"pass", company} }
                                  ) end),
    register( accounts , Pid).


%Users = #{ quim => {pass, investor}, pedro => {pass2,investor} , ...}
accounts( Map )->

    receive
        { verify , Username, Passwd, Pid } ->
            case maps:find( Username, Map) of
                {ok, {Passwd, Type} } ->
                    Pid ! {ok, Type} ;
                _ ->
                    Pid ! error
            end;

        { create, Username, Passwd, Type, Pid} ->
            case  maps:is_key(Username, Map) of
                true ->
                    Pid ! error;
                false ->
                    Pid ! ok,
                    Map = (maps:put( Username ,{Passwd, Type} , Map))
                    % enviar informaçao para o diretorio!!!
            end
    end,
    accounts(Map).





create_account( Username, Passwd, Type) ->
  accounts ! {create, Username, Passwd, Type, self()},
  receive
    V -> V
  end.

verify( Username, Passwd) ->
  accounts ! {verify, Username, Passwd, self()},
  receive
    V -> V
  end.
