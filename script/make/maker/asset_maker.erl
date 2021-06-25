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
    maker:start(fun parse_table/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_table({File, Table}) ->
    %% remove first column (`role_id`)
    [_ | NameList] = db:select("SELECT `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;", [Table]),
    [format_add_code(NameList, []), format_check_code(NameList, []), format_cost_code(NameList, []) | format_field_code(NameList, File, [], [], [])].

%% format record field, default value and comment
format_field_code([], File, AtomNameList, ExportNameList, Result) ->
    {ok, Forms} = epp:parse_file(File, [], []),
    OldExportList = lists:append([List || {attribute, _, export, List} <- Forms, lists:keyfind(hd(lists:reverse(AtomNameList)), 1, List) == {hd(lists:reverse(AtomNameList)), 1}]),
    %% export code
    ExportPattern = lists:concat(["\\-export\\(\\s*\\[\\s*", string:join([lists:concat([Name, "/", Arity]) || {Name, Arity} <- OldExportList], "\\s*,\\s*"), "\\s*\\]\\s*\\)\\."]),
    NewExportCode = lists:concat(["-export([", string:join(lists:reverse(ExportNameList), ", "), "])."]),
    %% field code
    CodePattern = lists:concat(["(?s)(?m)%%\\s*@doc\\s*", hd(lists:reverse(AtomNameList)), lists:duplicate(length(OldExportList), ".*?\\.$.*?\\.$"), "\\n?"]),
    NewFieldCode = string:join(lists:reverse(Result), "\n"),
    [{ExportPattern, NewExportCode}, {CodePattern, NewFieldCode}];
format_field_code([[Name] | T], File, AtomNameList, ExportNameList, Result) ->
    Function = io_lib:format("%% @doc ~s\n-spec ~s(#user{} | #asset{}) -> non_neg_integer().\n~s(#user{asset = #asset{~s = ~s}}) ->
    ~s;\n~s(#asset{~s = ~s}) ->
    ~s.\n", [Name, Name, Name, Name, word:to_hump(Name), word:to_hump(Name), Name, Name, word:to_hump(Name), word:to_hump(Name)]),
    format_field_code(T, File, [type:to_atom(Name) | AtomNameList], [io_lib:format("~s/1", [Name]) | ExportNameList], [Function | Result]).

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
    FinalUser = user_event:trigger(NewUser, #event{name = event_~s_add, target = ~s, number = NewNumber}),
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
            FinalUser = user_event:trigger(NewUser, #event{name = event_~s_cost, target = ~s, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{~s = ~s - NewNumber}}, T, From);
        _ ->
            {error, ~s}
    end;\n", [Name, word:to_hump(Name), Name, Name, word:to_hump(Name), Name, Name, Name, word:to_hump(Name), Name]),
    format_cost_code(T, [Function | Result]).
