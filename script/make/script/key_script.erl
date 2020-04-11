%%%-------------------------------------------------------------------
%%% @doc
%%% module key script
%%% @end
%%%-------------------------------------------------------------------
-module(key_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% extra shell param :
%%     -number       number specified
%%     -type         type specified
%%     -prefix       prefix specified
%% 
%%%===================================================================
%%% API functions
%%%===================================================================
main(T) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch key_maker:start(key(maker:parse_args(T)))]).

%%%===================================================================
%%% words data
%%%===================================================================
key(ArgList) ->
    Number = list_to_integer(hd(proplists:get_value("-number", ArgList, ["1"]))),
    Type = list_to_integer(hd(proplists:get_value("-type", ArgList, ["1"]))),
    Prefix = hd(proplists:get_value("-prefix", ArgList, [""])),
    Length = list_to_integer(hd(proplists:get_value("-length", ArgList, ["12"]))),
    [{"", key_data, Number, Type, Prefix, Length}].
