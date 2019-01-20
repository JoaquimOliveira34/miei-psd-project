-module(rest).

-export( [ getUsers/0, registerUser/3]).


getUsers()-> 
    inets:start(),
    {ok, Response} = httpc:request(get, {"http://localhost:8080/peerlending/users", []}, [], []),
    {_, _, JSON} = Response,
    parseUsers( split( JSON ), maps:new() ).

split( JSON ) ->
    string:tokens( JSON, ",:").

parseUsers( [] , Map ) ->  Map; 
parseUsers( List, Map ) ->
       [ _ | [ Name |  [_ | [Pass | [_ | [IsInvestor | [ _ |[ Id |T]]]]]]]] = List,
    
    case string:prefix( IsInvestor, "false") of
        nomatch -> Type = investor;
        _ -> Type = company
    end,
    FinalName = string:strip( Name, both, $"),
    FinalPass = string:strip( Pass, both, $"),
    {FinalId, _} = string:to_integer(Id),


    parseUsers( T, maps:put( FinalName, {FinalId, FinalPass, Type}, Map) ).


registerUser( Username, Passwd, Type )-> 
    inets:start(),
    case Type of
    
        investor ->
            Query = io_lib:format( "http://localhost:8080/peerlending/investor/~s/~s", [Username, Passwd]);
        company ->
            Query = io_lib:format( "http://localhost:8080/peerlending/company/~s/~s", [Username, Passwd])
    end,

    httpc:request( put, { Query , []}, [], []). 

