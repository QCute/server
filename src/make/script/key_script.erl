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
%%     -amount       amount specified
%%     -type         type specified
%%     -prefix       prefix specified
%% 
%%%===================================================================
%%% API
%%%===================================================================
main(T) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    maker:save_param_list(T),
    console:stacktrace(catch maker:start(fun key_maker:parse/2, key())),
    ok.

%%%===================================================================
%%% words data
%%%===================================================================
key() ->
    Amount = maker:find_param("-amount"),
    Type = maker:find_param("-type"),
    Prefix = maker:find_param("-prefix"),
    [{"", data_key, Amount, Type, Prefix}].