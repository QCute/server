%%%-------------------------------------------------------------------
%%% @doc
%%% application runtime journal define
%%% @end
%%%-------------------------------------------------------------------

%% print(no color)
-define(PRINT(Msg),                                   catch journal:print(?MODULE, ?LINE, Msg, [])).
-define(PRINT(Msg, Args),                             catch journal:print(?MODULE, ?LINE, Msg, Args)).
%% print only debug
-ifdef(DEBUG).
%% debug(blue)
-define(DEBUG_MSG(Msg),                               catch journal:debug(?MODULE, ?LINE, Msg, [])).
-define(DEBUG_MSG(Msg, Args),                         catch journal:debug(?MODULE, ?LINE, Msg, Args)).
-else.
%% debug(blue)
-define(DEBUG_MSG(Msg),                               ok).
-define(DEBUG_MSG(Msg, Args),                         ok).
-endif.
%% info(green)
-define(INFO_MSG(Msg),                                catch journal:info(?MODULE, ?LINE, Msg, [])).
-define(INFO_MSG(Msg, Args),                          catch journal:info(?MODULE, ?LINE, Msg, Args)).
%% warming(yellow)
-define(WARMING_MSG(Msg),                             catch journal:warming(?MODULE, ?LINE, Msg, [])).
-define(WARMING_MSG(Msg, Args),                       catch journal:warming(?MODULE, ?LINE, Msg, Args)).
%% error(red)
-define(ERROR_MSG(Msg),                               catch journal:error(?MODULE, ?LINE, Msg, [])).
-define(ERROR_MSG(Msg, Args),                         catch journal:error(?MODULE, ?LINE, Msg, Args)).

%% print stack trace message
-define(STACKTRACE(Reason, Stacktrace),               catch journal:print_stacktrace(?MODULE, ?LINE, Reason, Stacktrace)).

%% stack trace
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace),         Class:Reason:Stacktrace).
-define(GET_STACKTRACE(Stacktrace),                   Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _),                  Class:Reason).
-define(GET_STACKTRACE(_),                            erlang:get_stacktrace()).
-endif.
