-module(vip_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("vip.hrl").


level(Exp) when 200000 =< Exp ->
    15;
level(Exp) when 100000 =< Exp ->
    14;
level(Exp) when 60000 =< Exp ->
    13;
level(Exp) when 30000 =< Exp ->
    12;
level(Exp) when 10000 =< Exp ->
    11;
level(Exp) when 5000 =< Exp ->
    10;
level(Exp) when 3000 =< Exp ->
    9;
level(Exp) when 2000 =< Exp ->
    8;
level(Exp) when 1000 =< Exp ->
    7;
level(Exp) when 600 =< Exp ->
    6;
level(Exp) when 300 =< Exp ->
    5;
level(Exp) when 150 =< Exp ->
    4;
level(Exp) when 100 =< Exp ->
    3;
level(Exp) when 30 =< Exp ->
    2;
level(Exp) when 6 =< Exp ->
    1;
level(_) ->
    0.


