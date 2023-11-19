-module(level_data).
-export([min_level/0]).
-export([max_level/0]).
-export([level/1]).
-export([exp/1]).

-spec min_level() -> [non_neg_integer()].
min_level() ->
    0.

-spec max_level() -> [non_neg_integer()].
max_level() ->
    9.

-spec level(Exp :: non_neg_integer()) -> non_neg_integer().
level(Exp) when Exp < 200 ->
    0;
level(Exp) when Exp < 300 ->
    1;
level(Exp) when Exp < 400 ->
    2;
level(Exp) when Exp < 500 ->
    3;
level(Exp) when Exp < 600 ->
    4;
level(Exp) when Exp < 700 ->
    5;
level(Exp) when Exp < 800 ->
    6;
level(Exp) when Exp < 900 ->
    7;
level(Exp) when Exp < 1000 ->
    8;
level(_) ->
    0.

-spec exp(Level :: non_neg_integer()) -> non_neg_integer().
exp(0) ->
    100;
exp(1) ->
    200;
exp(2) ->
    300;
exp(3) ->
    400;
exp(4) ->
    500;
exp(5) ->
    600;
exp(6) ->
    700;
exp(7) ->
    800;
exp(8) ->
    900;
exp(9) ->
    1000;
exp(_) ->
    0.

