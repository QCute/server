-module(vip_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("vip.hrl").


level(Exp) when Exp >= 200000 ->
    15;
level(Exp) when Exp >= 100000 ->
    14;
level(Exp) when Exp >= 60000 ->
    13;
level(Exp) when Exp >= 30000 ->
    12;
level(Exp) when Exp >= 10000 ->
    11;
level(Exp) when Exp >= 5000 ->
    10;
level(Exp) when Exp >= 3000 ->
    9;
level(Exp) when Exp >= 2000 ->
    8;
level(Exp) when Exp >= 1000 ->
    7;
level(Exp) when Exp >= 600 ->
    6;
level(Exp) when Exp >= 300 ->
    5;
level(Exp) when Exp >= 150 ->
    4;
level(Exp) when Exp >= 100 ->
    3;
level(Exp) when Exp >= 30 ->
    2;
level(Exp) when Exp >= 6 ->
    1;
level(_Exp) ->
    0.


