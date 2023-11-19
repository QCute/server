%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% config script for config maker
%%% @end
%%%-------------------------------------------------------------------
-module(config_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
    try
        io:format("~tp~n", [config_maker:start(config())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main(Args) ->
    io:format(standard_error, "invalid argument: ~tp~n", [Args]).

%%%===================================================================
%%% config data
%%%===================================================================
config() ->
    [
        #{
            file => "src/tool/assistant/config.erl",
            config => "config/src/local.config.src"
        }
    ].
