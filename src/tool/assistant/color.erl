%%%-------------------------------------------------------------------
%%% @doc
%%% console print color tool
%%% @end
%%%-------------------------------------------------------------------
-module(color).
%% API
-export([black/1,   bright_black/1,   on_black/1]).
-export([red/1,     bright_red/1,     on_red/1]).
-export([green/1,   bright_green/1,   on_green/1]).
-export([blue/1,    bright_blue/1,    on_blue/1]).
-export([yellow/1,  bright_yellow/1,  on_yellow/1]).
-export([magenta/1, bright_magenta/1, on_magenta/1]).
-export([cyan/1,    bright_cyan/1,    on_cyan/1]).
-export([white/1,   bright_white/1,   on_white/1]).
-export([rgb/2,                       on_rgb/2]).
-export([true/2,                      on_true/2]).
%% Reset
-define(ESC,                          "\e[").
-define(RST,                          "0").
-define(BOLD,                         "1").
-define(SEP,                          ";").
-define(END,                          "m").

%% Colors
-define(BLACK,                        "30").
-define(RED,                          "31").
-define(GREEN,                        "32").
-define(YELLOW,                       "33").
-define(BLUE,                         "34").
-define(MAGENTA,                      "35").
-define(CYAN,                         "36").
-define(WHITE,                        "37").
-define(DEFAULT,                      "39").

%% Background colors
-define(BLACK_BG,                     "40").
-define(RED_BG,                       "41").
-define(GREEN_BG,                     "42").
-define(YELLOW_BG,                    "43").
-define(BLUE_BG,                      "44").
-define(MAGENTA_BG,                   "45").
-define(CYAN_BG,                      "46").
-define(WHITE_BG,                     "47").
-define(DEFAULT_BG,                   "49").

%% RGB
-define(RGB_FG,                       ["38", (?SEP), "5"]).
-define(RGB_BG,                       ["48", (?SEP), "5"]).

%% True 24-bit colors
-define(TRUE_COLOR_FG,                ["38", (?SEP), "2"]).
-define(TRUE_COLOR_BG,                ["48", (?SEP), "2"]).

-define(COLOR(Color),                 [?ESC, Color, ?END]).
-define(BRIGHT_COLOR(Color),          [?ESC, Color, ?SEP, ?BOLD, ?END]).
-define(RESET,                        [?ESC, ?RST, ?END]).
-define(RESET_BG,                     [?ESC, ?DEFAULT_BG, ?END]).

-define(RGB(R, G, B),                 integer_to_list(16 + (R * 36) + (G * 6) + B)).
-define(TRUE_COLOR(R, G, B),          [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec black(Text :: string()) -> string().
black(Text) ->
    [?COLOR(?BLACK), Text, ?RESET].

-spec bright_black(Text :: string()) -> string().
bright_black(Text) ->
    [?BRIGHT_COLOR(?BLACK), Text, ?RESET].

-spec red(Text :: string()) -> string().
red(Text) ->
    [?COLOR(?RED), Text, ?RESET].

-spec bright_red(Text :: string()) -> string().
bright_red(Text) ->
    [?BRIGHT_COLOR(?RED), Text, ?RESET].

-spec green(Text :: string()) -> string().
green(Text) ->
    [?COLOR(?GREEN), Text, ?RESET].

-spec bright_green(Text :: string()) -> string().
bright_green(Text) ->
    [?BRIGHT_COLOR(?GREEN), Text, ?RESET].

-spec yellow(Text :: string()) -> string().
yellow(Text) ->
    [?COLOR(?YELLOW), Text, ?RESET].

-spec bright_yellow(Text :: string()) -> string().
bright_yellow(Text) ->
    [?BRIGHT_COLOR(?YELLOW), Text, ?RESET].

-spec blue(Text :: string()) -> string().
blue(Text) ->
    [?COLOR(?BLUE), Text, ?RESET].

-spec bright_blue(Text :: string()) -> string().
bright_blue(Text) ->
    [?BRIGHT_COLOR(?BLUE), Text, ?RESET].

-spec magenta(Text :: string()) -> string().
magenta(Text) ->
    [?COLOR(?MAGENTA), Text, ?RESET].

-spec bright_magenta(Text :: string()) -> string().
bright_magenta(Text) ->
    [?BRIGHT_COLOR(?MAGENTA), Text, ?RESET].

-spec cyan(Text :: string()) -> string().
cyan(Text) ->
    [?COLOR(?CYAN), Text, ?RESET].

-spec bright_cyan(Text :: string()) -> string().
bright_cyan(Text) ->
    [?BRIGHT_COLOR(?CYAN), Text, ?RESET].

-spec white(Text :: string()) -> string().
white(Text) ->
    [?COLOR(?WHITE), Text, ?RESET].

-spec bright_white(Text :: string()) -> string().
bright_white(Text) ->
    [?BRIGHT_COLOR(?WHITE), Text, ?RESET].

-spec rgb(RGB :: {0..5, 0..5, 0..5}, Text :: string()) -> string().
rgb({R, G, B}, Text) ->
    [?ESC, ?RGB_FG, ?SEP, ?RGB(R, G, B), ?END, Text, ?RESET].

-spec true(RGB :: {0..255, 0..255, 0..255}, Text :: string()) -> string().
true({R, G, B}, Text) ->
    [?ESC, ?TRUE_COLOR_FG, ?SEP, ?TRUE_COLOR(R, G, B), ?END, Text, ?RESET].

-spec on_black(Text :: string()) -> string().
on_black(Text) ->
    [?COLOR(?BLACK_BG), Text, ?RESET_BG].

-spec on_red(Text :: string()) -> string().
on_red(Text) ->
    [?COLOR(?RED_BG), Text, ?RESET_BG].

-spec on_green(Text :: string()) -> string().
on_green(Text) ->
    [?COLOR(?GREEN_BG), Text, ?RESET_BG].

-spec on_blue(Text :: string()) -> string().
on_blue(Text) ->
    [?COLOR(?BLUE_BG), Text, ?RESET_BG].

-spec on_yellow(Text :: string()) -> string().
on_yellow(Text) ->
    [?COLOR(?YELLOW_BG), Text, ?RESET_BG].

-spec on_magenta(Text :: string()) -> string().
on_magenta(Text) ->
    [?COLOR(?MAGENTA_BG), Text, ?RESET_BG].

-spec on_cyan(Text :: string()) -> string().
on_cyan(Text) ->
    [?COLOR(?CYAN_BG), Text, ?RESET_BG].

-spec on_white(Text :: string()) -> string().
on_white(Text) ->
    [?COLOR(?WHITE_BG), Text, ?RESET_BG].

-spec on_rgb(RGB :: {0..5, 0..5, 0..5}, Text :: string()) -> string().
on_rgb({R, G, B}, Text) ->
    [?ESC, ?RGB_FG, ?SEP, ?RGB(R, G, B), ?END, Text, ?RESET_BG].

-spec on_true(RGB :: {0..255, 0..255, 0..255}, Text :: string()) -> string().
on_true({R, G, B}, Text) ->
    [?ESC, ?TRUE_COLOR_BG, ?SEP, ?TRUE_COLOR(R, G, B), ?END, Text, ?RESET_BG].

%%%===================================================================
%%% Internal functions
%%%===================================================================
