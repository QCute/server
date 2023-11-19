-ifndef(COMMON_HRL).
-define(COMMON_HRL, 'COMMON_HRL').

%% boolean define
-define(TRUE,                                         1).
-define(FALSE,                                        0).

%% common error type define
-type error() :: error | {error, term()}.

-endif.
