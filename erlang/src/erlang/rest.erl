-module(rest).

-export([ getUsers/0, registerUser/4 ]).


getUsers()-> #{ "quim" => { 0,"pass", investor}, 
                "pedro" => {1, "pass", company}
              }.

registerUser( Username, Passwd, Type, Id  )-> ok.
