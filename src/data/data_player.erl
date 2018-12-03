-module(data_player).
-compile(nowarn_export_all).
-compile(export_all).
-include("player.hrl").


level(Exp) when Exp < 100 ->
    0;
level(Exp) when Exp < 200 ->
    1;
level(Exp) when Exp < 300 ->
    2;
level(Exp) when Exp < 400 ->
    3;
level(Exp) when Exp < 500 ->
    4;
level(Exp) when Exp < 600 ->
    5;
level(Exp) when Exp < 700 ->
    6;
level(Exp) when Exp < 800 ->
    7;
level(Exp) when Exp < 900 ->
    8;
level(Exp) when Exp < 1000 ->
    9;
level(_) -> 
    0.

