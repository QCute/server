%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% asset script for asset maker
%%% @end
%%%-------------------------------------------------------------------
-module(asset_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [asset_maker:start(asset())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% asset data
%%%===================================================================
asset() ->
    [
        #{
            file => "src/module/asset/asset.erl",
            table => asset
        }
    ].
