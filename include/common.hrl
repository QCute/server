%%%-------------------------------------------------------------------
%%% @doc
%%% common define
%%% @end
%%%-------------------------------------------------------------------
%% boolean define
-define(TRUE,                                         1).
-define(FALSE,                                        0).

%% common error type define
-type error() :: error | {error, term()}.
