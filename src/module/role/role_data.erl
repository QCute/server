-module(role_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").


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

vip(Gold) when Gold < 6 ->
    1;
vip(Gold) when Gold < 30 ->
    2;
vip(Gold) when Gold < 100 ->
    3;
vip(Gold) when Gold < 150 ->
    4;
vip(Gold) when Gold < 300 ->
    5;
vip(Gold) when Gold < 600 ->
    6;
vip(Gold) when Gold < 1000 ->
    7;
vip(Gold) when Gold < 2000 ->
    8;
vip(Gold) when Gold < 3000 ->
    9;
vip(Gold) when Gold < 5000 ->
    10;
vip(Gold) when Gold < 10000 ->
    11;
vip(Gold) when Gold < 30000 ->
    12;
vip(Gold) when Gold < 60000 ->
    13;
vip(Gold) when Gold < 100000 ->
    14;
vip(Gold) when Gold < 200000 ->
    15;
vip(_) -> 
    0.

