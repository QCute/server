-module(vip_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("vip.hrl").


vip(Exp) when Exp < 6 ->
    1;
vip(Exp) when Exp < 30 ->
    2;
vip(Exp) when Exp < 100 ->
    3;
vip(Exp) when Exp < 150 ->
    4;
vip(Exp) when Exp < 300 ->
    5;
vip(Exp) when Exp < 600 ->
    6;
vip(Exp) when Exp < 1000 ->
    7;
vip(Exp) when Exp < 2000 ->
    8;
vip(Exp) when Exp < 3000 ->
    9;
vip(Exp) when Exp < 5000 ->
    10;
vip(Exp) when Exp < 10000 ->
    11;
vip(Exp) when Exp < 30000 ->
    12;
vip(Exp) when Exp < 60000 ->
    13;
vip(Exp) when Exp < 100000 ->
    14;
vip(Exp) when Exp < 200000 ->
    15;
vip(_) ->
    0.


more(Exp) when 200000 =< Exp ->
    15;
more(Exp) when 100000 =< Exp ->
    14;
more(Exp) when 60000 =< Exp ->
    13;
more(Exp) when 30000 =< Exp ->
    12;
more(Exp) when 10000 =< Exp ->
    11;
more(Exp) when 5000 =< Exp ->
    10;
more(Exp) when 3000 =< Exp ->
    9;
more(Exp) when 2000 =< Exp ->
    8;
more(Exp) when 1000 =< Exp ->
    7;
more(Exp) when 600 =< Exp ->
    6;
more(Exp) when 300 =< Exp ->
    5;
more(Exp) when 150 =< Exp ->
    4;
more(Exp) when 100 =< Exp ->
    3;
more(Exp) when 30 =< Exp ->
    2;
more(Exp) when 6 =< Exp ->
    1;
more(_) ->
    0.


