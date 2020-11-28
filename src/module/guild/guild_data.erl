-module(guild_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").


create_type(0) ->
    {0, [], []};
create_type(1) ->
    {1, [{level, 1}, {vip, 1}], [{100001, 1}]};
create_type(2) ->
    {2, [{level, 2}, {vip, 2}], [{100001, 2}]};
create_type(3) ->
    {3, [{level, 3}, {vip, 3}], [{100001, 3}]};
create_type(_) ->
    [].


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
level(_) ->
    0.


