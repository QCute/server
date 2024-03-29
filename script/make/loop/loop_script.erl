%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% loop script to loop maker
%%% @end
%%%-------------------------------------------------------------------
-module(loop_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%% ------------------------ user guide -------------------------------
%% loop name [-create|-load|-save|-reset|-clean|-expire|-login|-logout|-reconnect|-disconnect|-after field name|-comment the field comment]
%% name         : field name
%% -create      : create interface supported
%% -load        : load interface supported
%% -save        : save interface supported
%% -reset       : reset interface supported
%% -clean       : clean interface supported
%% -expire      : expire interface supported
%% -login       : login interface supported
%% -logout      : logout interface supported
%% -reconnect   : reconnect interface supported
%% -disconnect  : disconnect interface supported
%% -after       : after field
%% -comment     : module comment
%%%===================================================================
%%% API functions
%%%===================================================================
main(Args) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [loop_maker:start(loop(Args))])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% loop options
%%%===================================================================
loop(Args) ->
    [
        #{
            file => "src/module/user/user_loop_create.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_load.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_save.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_reset.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_clean.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_expire.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_login.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_logout.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_reconnect.erl",
            header => "include/user.hrl",
            args => Args
        },
        #{
            file => "src/module/user/user_loop_disconnect.erl",
            header => "include/user.hrl",
            args => Args
        }
    ].
