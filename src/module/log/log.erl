%%%-------------------------------------------------------------------
%%% @doc
%%% module log
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API
%%%===================================================================
sql(log_player) ->
    {"INSERT INTO `log_player` (`user_id`, `exp`, `time`) VALUES ", "('~w', '~w', '~w')"};
sql(_) ->
    ok.

log_player(UserId, Exp, Time) ->
    log_server:log(log_player, [UserId, Exp, Time]).

