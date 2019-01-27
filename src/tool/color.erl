%%%-------------------------------------------------------------------
%%% @doc
%%% module color (for console print)
%%% @end
%%%-------------------------------------------------------------------
-module(color).
-export([black/1,    bright_black/1,    on_black/1]).
-export([red/1,      bright_red/1,      on_red/1]).
-export([green/1,    bright_green/1,    on_green/1]).
-export([blue/1,     bright_blue/1,     on_blue/1]).
-export([yellow/1,   bright_yellow/1,   on_yellow/1]).
-export([magenta/1,  bright_magenta/1,  on_magenta/1]).
-export([cyan/1,     bright_cyan/1,     on_cyan/1]).
-export([white/1,    bright_white/1,    on_white/1]).
-export([rgb/2,      on_rgb/2]).
-export([true/2,     on_true/2]).

%% Reset
-define(ESC,             <<"\e[">>).
-define(RST,             <<"0">>).
-define(BOLD,            <<"1">>).
-define(SEP,             <<";">>).
-define(END,             <<"m">>).

%% Colors
-define(BLACK,           <<"30">>).
-define(RED,             <<"31">>).
-define(GREEN,           <<"32">>).
-define(YELLOW,          <<"33">>).
-define(BLUE,            <<"34">>).
-define(MAGENTA,         <<"35">>).
-define(CYAN,            <<"36">>).
-define(WHITE,           <<"37">>).
-define(DEFAULT,         <<"39">>).

%% Background colors
-define(BLACK_BG,        <<"40">>).
-define(RED_BG,          <<"41">>).
-define(GREEN_BG,        <<"42">>).
-define(YELLOW_BG,       <<"43">>).
-define(BLUE_BG,         <<"44">>).
-define(MAGENTA_BG,      <<"45">>).
-define(CYAN_BG,         <<"46">>).
-define(WHITE_BG,        <<"47">>).
-define(DEFAULT_BG,      <<"49">>).

%% RGB
-define(RGB_FG,          [<<"38">>, ?SEP, <<"5">>]).
-define(RGB_BG,          [<<"48">>, ?SEP, <<"5">>]).

%% True 24-bit colors
-define(TRUE_COLOR_FG,   [<<"38">>, ?SEP, <<"2">>]).
-define(TRUE_COLOR_BG,   [<<"48">>, ?SEP, <<"2">>]).

%%====================================================================
%% API functions
%%====================================================================
black(Text)          -> [color(?BLACK),            thing_to_list(Text), reset()].
bright_black(Text)   -> [bright_color(?BLACK),     thing_to_list(Text), reset()].
red(Text)            -> [color(?RED),              thing_to_list(Text), reset()].
bright_red(Text)     -> [bright_color(?RED),       thing_to_list(Text), reset()].
green(Text)          -> [color(?GREEN),            thing_to_list(Text), reset()].
bright_green(Text)   -> [bright_color(?GREEN),     thing_to_list(Text), reset()].
yellow(Text)         -> [color(?YELLOW),           thing_to_list(Text), reset()].
bright_yellow(Text)  -> [bright_color(?YELLOW),    thing_to_list(Text), reset()].
blue(Text)           -> [color(?BLUE),             thing_to_list(Text), reset()].
bright_blue(Text)    -> [bright_color(?BLUE),      thing_to_list(Text), reset()].
magenta(Text)        -> [color(?MAGENTA),          thing_to_list(Text), reset()].
bright_magenta(Text) -> [bright_color(?MAGENTA),   thing_to_list(Text), reset()].
cyan(Text)           -> [color(?CYAN),             thing_to_list(Text), reset()].
bright_cyan(Text)    -> [bright_color(?CYAN),      thing_to_list(Text), reset()].
white(Text)          -> [color(?WHITE),            thing_to_list(Text), reset()].
bright_white(Text)   -> [bright_color(?WHITE),     thing_to_list(Text), reset()].
on_black(Text)       -> [color(?BLACK_BG),         thing_to_list(Text), reset_bg()].
on_red(Text)         -> [color(?RED_BG),           thing_to_list(Text), reset_bg()].
on_green(Text)       -> [color(?GREEN_BG),         thing_to_list(Text), reset_bg()].
on_blue(Text)        -> [color(?BLUE_BG),          thing_to_list(Text), reset_bg()].
on_yellow(Text)      -> [color(?YELLOW_BG),        thing_to_list(Text), reset_bg()].
on_magenta(Text)     -> [color(?MAGENTA_BG),       thing_to_list(Text), reset_bg()].
on_cyan(Text)        -> [color(?CYAN_BG),          thing_to_list(Text), reset_bg()].
on_white(Text)       -> [color(?WHITE_BG),         thing_to_list(Text), reset_bg()].

rgb(RGB, Text) ->
    [?ESC, ?RGB_FG, ?SEP, rgb_color(RGB), ?END, Text, reset()].

on_rgb(RGB, Text) ->
    [?ESC, ?RGB_BG, ?SEP, rgb_color(RGB), ?END, Text, reset_bg()].

true(RGB, Text) ->
    [?ESC, ?TRUE_COLOR_FG, ?SEP, true_color(RGB), ?END, Text, reset()].

on_true(RGB, Text) ->
    [?ESC, ?TRUE_COLOR_BG, ?SEP, true_color(RGB), ?END, Text, reset()].

%%====================================================================
%% Internal functions
%%====================================================================
color(Color) ->
    <<?ESC/binary, Color/binary, ?END/binary>>.

bright_color(Color) ->
    <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

rgb_color([R, G, B]) when R >= 0, R =< 5, G >= 0, G =< 5, B >= 0, B =< 5 ->
    integer_to_list(16 + (R * 36) + (G * 6) + B).

true_color([R1, R2, G1, G2, B1, B2]) ->
    R = erlang:list_to_integer([R1, R2], 16),
    G = erlang:list_to_integer([G1, G2], 16),
    B = erlang:list_to_integer([B1, B2], 16),
    true_color([R, G, B]);

true_color([R, G, B]) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
    [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)].

reset() ->
    <<?ESC/binary, ?RST/binary, ?END/binary>>.

reset_bg() ->
    <<?ESC/binary, ?DEFAULT_BG/binary, ?END/binary>>.

%% data type convert
thing_to_list(X) when is_integer(X) -> io_lib:format("~w", [X]);
thing_to_list(X) when is_binary(X)  -> io_lib:format("~w", [X]);
thing_to_list(X) when is_tuple(X)   -> io_lib:format("~w", [X]);
thing_to_list(X) when is_atom(X)    -> io_lib:format("~w", [X]);
thing_to_list(X) when is_list(X)    -> X.
