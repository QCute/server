%%%-------------------------------------------------------------------
%%% @doc
%%% module config maker
%%% @end
%%%-------------------------------------------------------------------
-module(config_maker).
-export([start/2]).
%%====================================================================
%% table to excel
%%====================================================================
start(InFile, OutFile) ->
    case file:consult(InFile) of
        {ok, [Terms]} ->
            %% without sasl and kernel config
            All = [make_function(Name, List) || {Name, List} <- Terms, Name =/= sasl andalso Name =/= kernel],
            Head = "-module(config).\n-compile(nowarn_export_all).\n-compile(export_all).\n\n\n",
            file:write_file(OutFile, Head ++ lists:flatten(All));
        Error ->
            Error
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% k/v type
make_function(Name, List) ->
    [make_loop(Name, Key, Value) || {Key, Value} <- List].

%% sub value is k/v type
make_loop(_Name, Key, Value = [{_, _} | _]) ->
    make_function(Key, Value);
%% sub value not k/v type
make_loop(Name, Key, Value) ->
    io_lib:format("~s() ->\n    case application:get_env(~s, ~s) of\n        {ok, Value} ->\n            Value;\n        _ ->\n            ~p\n    end.\n\n", [Key, Name, Key, Value]).
