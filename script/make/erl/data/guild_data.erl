-module(guild_data).
-export([create_type/1]).
-export([level/1]).

-spec create_type(Level :: non_neg_integer()) -> {non_neg_integer(), term(), term()}.
create_type(0) ->
    {0, [], []};
create_type(1) ->
    {1, [{level, 1}, {vip, 1}], [{100001, 1}]};
create_type(2) ->
    {2, [{level, 2}, {vip, 2}], [{100001, 2}]};
create_type(3) ->
    {3, [{level, 3}, {vip, 3}], [{100001, 3}]};
create_type(_) ->
    undefined.

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

