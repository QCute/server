%%%-------------------------------------------------------------------
%%% @doc
%%% loop script to loop maker
%%% @end
%%%-------------------------------------------------------------------
-module(loop_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%% -load        : load interface supported
%% -save        : save interface supported
%% -reset       : reset interface supported
%% -clean       : clean interface supported
%% -expire      : expire interface supported
%% -login       : login interface supported
%% -logout      : logout interface supported
%% -reconnect   : reconnect interface supported
%% -disconnect  : disconnect interface supported
%% -comment : module comment
%%%===================================================================
%%% API functions
%%%===================================================================
main(Args) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = loop_maker:start([{"src/module/user/user_loop.erl", "include/user.hrl", Args}]),
    io:format("~p~n", [Result]).
