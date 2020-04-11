%%%-------------------------------------------------------------------
%%% @doc
%%% module record script
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% default value guide
%% varchar/char                             => <<>>
%% varchar/char with (convert) specified    => []
%% varchar/char with (null) specified       => undefined
%% varchar/char with (number) specified     => number
%% 
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch attribute_maker:start(attribute())]).

%%%===================================================================
%%% record data
%%%===================================================================
attribute() ->
    [
        {"src/module/attribute/attribute.erl", attribute_data, "attribute"},
        {"include/attribute.hrl", attribute_data, "attribute"}
    ].
