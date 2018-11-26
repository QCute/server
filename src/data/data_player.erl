-module(data_player).
-compile(nowarn_export_all).
-compile(export_all).
-include("player.hrl").


level(Exp) when Exp < 100 ->
    1;
level(Exp) when Exp < 200 ->
    2;
level(Exp) when Exp < 300 ->
    3;
level(Exp) when Exp < 400 ->
    4;
level(Exp) when Exp < 500 ->
    5;
level(Exp) when Exp < 600 ->
    6;
level(_) -> 
    0.

