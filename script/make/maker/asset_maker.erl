%%%-------------------------------------------------------------------
%%% @doc
%%% make asset table field to asset add/check/cost code
%%% @end
%%%-------------------------------------------------------------------
-module(asset_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_table(DataBase, {_, Table}) ->
    %% remove first column (`role_id`)
    [_ | NameList] = maker:select(io_lib:format("SELECT `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;", [DataBase, Table])),
    [format_add_code(NameList, []), format_check_code(NameList, []), format_cost_code(NameList, [])].

%% format record field, default value and comment
format_add_code([], Result) ->
    {"(?m)(?s)^add\\s*\\(.*?\\.$", "add(User, [], _) ->
    %% push after add
    push(User),
    {ok, User};\n" ++ string:join(lists:reverse(Result), "") ++ "add(_, [{Type, _} | _], _) ->
    {error, Type}."};
format_add_code([[Name] | T], Result) ->
    Function = io_lib:format("add(User = #user{asset = Asset = #asset{~s = ~s}}, [{~s, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, ~s, Number, From),
    FinalUser = user_event:handle(NewUser, #event{name = event_~s_add, target = ~s, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{~s = ~s + NewNumber}}, T, From);\n", [Name, word:to_hump(Name), Name, Name, Name, Name, Name, word:to_hump(Name)]),
    format_add_code(T, [Function | Result]).

format_check_code([], Result) ->
    {"(?m)(?s)^check\\s*\\(.*?\\.$", "check(_, [], _) ->
    ok;\n" ++ string:join(lists:reverse(Result), "") ++ "check(_, [{Type, _} | _], _) ->
    {error, Type}."};
format_check_code([[Name] | T], Result) ->
    Function = io_lib:format("check(User = #user{asset = Asset = #asset{~s = ~s}}, [{~s, Number} | T], From) ->
    case Number =< ~s of
        true ->
            check(User#user{asset = Asset#asset{~s = ~s - Number}}, T, From);
        _ ->
            {error, ~s}
    end;\n", [Name, word:to_hump(Name), Name, word:to_hump(Name), Name, word:to_hump(Name), Name]),
    format_check_code(T, [Function | Result]).

format_cost_code([], Result) ->
    {"(?m)(?s)^cost\\s*\\(.*?\\.$", "cost(User, [], _) ->
    %% push after cost
    push(User),
    {ok, User};\n" ++ string:join(lists:reverse(Result), "") ++ "cost(_, [{Type, _} | _], _) ->
    {error, Type}."};
format_cost_code([[Name] | T], Result) ->
    Function = io_lib:format("cost(User = #user{asset = Asset = #asset{~s = ~s}}, [{~s, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, ~s, Number, From) of
        {NewUser, NewNumber} when NewNumber =< ~s ->
            FinalUser = user_event:handle(NewUser, #event{name = event_~s_cost, target = ~s, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{~s = ~s - NewNumber}}, T, From);
        _ ->
            {error, ~s}
    end;\n", [Name, word:to_hump(Name), Name, Name, word:to_hump(Name), Name, Name, Name, word:to_hump(Name), Name]),
    format_cost_code(T, [Function | Result]).
