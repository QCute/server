-module(data_vip).
-compile(nowarn_export_all).
-compile(export_all).


get(Money) when Money < 30 ->
    1;
get(Money) when Money < 100 ->
    2;
get(Money) when Money < 256 ->
    3;
get(Money) when Money < 580 ->
    4;
get(Money) when Money < 1000 ->
    5;
get(Money) when Money < 4000 ->
    6;
get(Money) when Money < 10000 ->
    7;
get(Money) when Money < 30000 ->
    8;
get(Money) when Money < 100000 ->
    9;
get(Money) when Money < 200000 ->
    10;
get(Money) when Money < 400000 ->
    11;
get(_) -> 
    0.

list() ->
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11].

