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
%%% API
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch maker:start(fun attribute_maker:parse/2, attribute())).

%%%===================================================================
%%% record data
%%%===================================================================
attribute() ->
    [
        {"src/module/attribute/attribute.erl", data_attribute, "attribute"},
        {"include/attribute.hrl", data_attribute, "attribute"}
    ].
