%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% key script for key maker
%%% @end
%%%-------------------------------------------------------------------
-module(key_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%% ------------------------ user guide -------------------------------
%%
%% usage : [-n <number>] [-t <type>] [-p <prefix>] [-l <length>]
%%     -n --number                number of key generate
%%     -t --type                  key award type
%%     -p --prefix                key prefix
%%     -l --length                key length
%%
%%%===================================================================
%%% API functions
%%%===================================================================
main(T) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [key_maker:start(key(maker:parse_args(T)))])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% key options
%%%===================================================================
key(ArgList) ->
    Number = list_to_integer(hd(proplists:get_value("number", ArgList, ["1"]))),
    Type = list_to_integer(hd(proplists:get_value("type", ArgList, ["1"]))),
    Prefix = hd(proplists:get_value("prefix", ArgList, [""])),
    Length = list_to_integer(hd(proplists:get_value("length", ArgList, ["12"]))),
    {"", key_data, Number, Type, Prefix, Length}.
