-module(role_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("role.hrl").


min_level() ->
    0.


max_level() ->
    9.


level(Exp) when Exp > 1000 ->
    9;
level(Exp) when Exp > 900 ->
    8;
level(Exp) when Exp > 800 ->
    7;
level(Exp) when Exp > 700 ->
    6;
level(Exp) when Exp > 600 ->
    5;
level(Exp) when Exp > 500 ->
    4;
level(Exp) when Exp > 400 ->
    3;
level(Exp) when Exp > 300 ->
    2;
level(Exp) when Exp > 200 ->
    1;
level(Exp) when Exp > 100 ->
    0;
level(_Exp) ->
    0.


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
exp(_Level) ->
    0.


sex(1) ->
    <<"男"/utf8>>;
sex(2) ->
    <<"女"/utf8>>;
sex(_Sex) ->
    <<>>.


classes(1) ->
    <<"七杀"/utf8>>;
classes(2) ->
    <<"天师"/utf8>>;
classes(3) ->
    <<"飞羽"/utf8>>;
classes(4) ->
    <<"御灵"/utf8>>;
classes(5) ->
    <<"妙音"/utf8>>;
classes(6) ->
    <<"星术"/utf8>>;
classes(_Classes) ->
    <<>>.


