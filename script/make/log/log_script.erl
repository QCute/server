%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% log script for log maker
%%% @end
%%%-------------------------------------------------------------------
-module(log_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
    try
        io:format("~tp~n", [log_maker:start(log())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        #{
            file => "script/make/log/data/log.erl",
            type => log
        },
        #{
            file => "script/make/log/data/log_save.erl",
            type => save
        },
        #{
            file => "script/make/log/data/log_delete.erl",
            type => delete,
            expire => 259200
        },
        #{
            file => "script/make/log/data/log_delete_return.erl",
            type => delete_return,
            expire => 259200
        },
        #{
            file => "script/make/log/data/log_replace.erl",
            type => replace
        }
    ].
