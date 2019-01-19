-module(rest).

-export( [ getUsers/0, registerUser/4 ]).


getUsers()-> #{ "quim" => { 0,"pass", investor}, 
                "pedro" => {1, "pass", company}
              }.

gett()-> 
    inets:start(),
    {ok, Response} = httpc:request(get, {"http://localhost:8080/peerlending/users", []}, [], []),
    {_, _, JSON} = Response,
    print( split(JSON)),
    parseUsers( split( JSON ), maps:new() ).

split( JSON ) ->
    string:tokens( JSON, ",:").

parseUsers( [] , Map ) ->  Map; 
parseUsers( List, Map ) ->
    [ _ | [ Pass |  [_ | [Name | [_ | [IsInvestor | T]]]]]] = List,
    
    case string:prefix( IsInvestor, "false") of
        nomatch -> Type = company;
        _ -> Type = investor
    end,

    FinalName = string:strip( Name, both, $"),
    FinalPass = string:strip( Pass, both, $"),
    
    Id = 22,

    parseUsers( T, maps:put( FinalName, {Id, FinalPass, Type}, Map) ).


registerUser( Username, Passwd, Type, Id  )-> 
    
    case Type of
    
        investor ->
            Query = io_lib:format( "http://localhost:8080/peerlending/investor/~s/~s/~d", [Username, Passwd, Id]);
        company ->
            Query = io_lib:format( "http://localhost:8080/peerlending/company/~s/~s/~d", [Username, Passwd, Id])
    end,

    httpc:request( put, { Query , []}, [], []). 


print( [] ) -> ok;
print( List ) -> 
    
    [ _ | [ Pass |  [_ | [Name | [_ | [IsInvestor | T]]]]]] = List,
    
    case string:prefix( IsInvestor, "false") of
        nomatch -> Type = company;
        _ -> Type = investor
    end,
    FinalName = string:strip( Name, both, $"),
    FinalPass = string:strip( Pass, both, $"),
    
    io:format( " Type:|~s|, Name:|~s| Pass:|~s|\n", [ Type, FinalName, FinalPass]),  
    
    print(T).

