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

