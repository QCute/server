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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [config_maker:start("config/src/local.config.src", "src/tool/assistant/config.erl")])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main([File]) ->
    try
        format(File)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main(Args) ->
    io:format(standard_error, "invalid argument: ~tp~n", [Args]).

%% format config file
format(File) ->
    {ok, [Config]} = file:consult(File),
    file:write_file(File, lists:concat([format_config(Config, 1, []), "."])).
format_config([], Depth, String) ->
    Padding = lists:concat(lists:duplicate((Depth - 1) * 4, " ")),
    lists:concat(["[\n", string:join(lists:reverse(String), ",\n"), "\n", Padding, "]"]);
format_config([{Key, Value} | T], Depth, String) ->
    case is_list(Value) andalso lists:any(fun(C) -> is_tuple(C) end, Value) of
        true ->
            Padding = lists:concat(lists:duplicate(Depth * 4, " ")),
            NewString = io_lib:format("~s{~p, ~s}", [Padding, Key, format_config(Value, Depth + 1, [])]),
            format_config(T, Depth, [NewString | String]);
        false when Value == [] ->
            Padding = lists:concat(lists:duplicate(Depth * 4, " ")),
            Align = lists:concat(lists:duplicate(53 - (Depth * 4 + 1 + length(lists:concat([Key]))), " ")),
            NewString = io_lib:format("~s{~p,~s\"\"}", [Padding, Key, Align]),
            format_config(T, Depth, [NewString | String]);
        false ->
            Padding = lists:concat(lists:duplicate(Depth * 4, " ")),
            Align = lists:concat(lists:duplicate(53 - (Depth * 4 + 1 + length(lists:concat([Key]))), " ")),
            NewString = io_lib:format("~s{~p,~s~p}", [Padding, Key, Align, Value]),
            format_config(T, Depth, [NewString | String])
    end.
