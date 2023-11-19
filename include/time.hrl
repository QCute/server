-ifndef(TIME_HRL).
-define(TIME_HRL, 'TIME_HRL').

%% milliseconds for timer
-define(SECOND_MILLISECONDS(Seconds),                 ((Seconds) * 1000)).       %% number milliseconds of seconds
-define(MINUTE_MILLISECONDS(Minute),                  ((Minute) * 60 * 1000)).   %% number milliseconds of minutes
-define(HOUR_MILLISECONDS(Hour),                      ((Hour) * 3600 * 1000)).   %% number milliseconds of hours
-define(DAY_MILLISECONDS(Day),                        ((Day) * 86400 * 1000)).   %% number milliseconds of days
-define(WEEK_MILLISECONDS(Week),                      ((Week) * 604800 * 1000)). %% number milliseconds of weeks

%% second normally use
-define(MINUTE_SECONDS(Minute),                       ((Minute) * 60)).          %% number seconds of minutes
-define(HOUR_SECONDS(Hour),                           ((Hour) * 3600)).          %% number seconds of hours
-define(DAY_SECONDS(Day),                             ((Day) * 86400)).          %% number seconds of days
-define(WEEK_SECONDS(Week),                           ((Week) * 604800)).        %% number seconds of weeks

%% milliseconds for timer
-define(SECOND_MILLISECONDS,                          (1000)).                   %% number milliseconds of second
-define(MINUTE_MILLISECONDS,                          (60 * 1000)).              %% number milliseconds of minute
-define(HOUR_MILLISECONDS,                            (3600 * 1000))).           %% number milliseconds of hour
-define(DAY_MILLISECONDS,                             (86400 * 1000)).           %% number milliseconds of day
-define(WEEK_MILLISECONDS,                            (604800 * 1000)).          %% number milliseconds of week

%% second normally use
-define(MINUTE_SECONDS,                               (60)).                     %% number seconds of minute
-define(HOUR_SECONDS,                                 (3600)).                   %% number seconds of hour
-define(DAY_SECONDS,                                  (86400)).                  %% number seconds of day
-define(WEEK_SECONDS,                                 (604800)).                 %% number seconds of week

-define(CALL_TIMEOUT,                                 ?SECOND_MILLISECONDS(5)).  %% default call timeout

-endif.
