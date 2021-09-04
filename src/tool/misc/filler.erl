%%%-------------------------------------------------------------------
%%% @doc
%%% database data filler tool
%%% @end
%%%-------------------------------------------------------------------
-module(filler).
%% API
-export([main/1]).
-export([fill/0, fill/1]).
-include("journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% script
-spec main(Args :: [term()]) -> ok.
main(_) ->
    maker:connect_database(),
    fill().

%% fill data
-spec fill() -> ok.
fill() ->
    Tables = fetch_tables(),
    lists:foreach(fun(Table) -> fill(Table) end, Tables).

%% fill table data
-spec fill(Table :: binary()) -> ok.
fill(Table) ->
    try
        Columns = fetch_columns(Table),
        {Names, Fill} = format_row(Columns, [], []),
        Format = {<<"INSERT INTO ", "`", Table/binary, "`", "(", (join(Names))/binary, ")", " VALUES ">>, <<"(", (join(lists:duplicate(length(Fill), <<"'~s'">>)))/binary, ")">>},
        persistent_term:put(Table, atomics:new(1, [{signed, false}])),
        Rows = table_fill_rows(binary_to_atom(Table, utf8)),
        lists:foreach(fun(_) -> db:insert(parser:collect([[apply(F, [Table]) || F <- Fill] || <<_:8>> <= binary:copy(<<0>>, 10000)], Format)), erlang:garbage_collect(self()) end, lists:seq(1, Rows)),
        persistent_term:erase(Table),
        erlang:garbage_collect(self())
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end.

%%%===================================================================
%%% Tables
%%%===================================================================
%% control fill number rows 10k times
table_fill_rows(role) -> 1;
table_fill_rows(_) -> 10.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% not data table
fetch_tables() ->
    db:select_column("SELECT `TABLE_NAME` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%_data' ORDER BY `TABLE_NAME`").

%% not auto increment, virtual column
fetch_columns(Table) ->
    db:select("SELECT `COLUMN_KEY`, CONCAT('`', `COLUMN_NAME`, '`') AS `COLUMN_NAME`, `NUMERIC_PRECISION`, `CHARACTER_MAXIMUM_LENGTH` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' AND `EXTRA` != 'auto_increment' AND `IS_GENERATED` = 'NEVER' ORDER BY `ORDINAL_POSITION`", [Table]).

%%%===================================================================
%%% Data filler
%%%===================================================================

format_row([], Fill, Name) ->
    {Name, Fill};

%% integer type primary key
format_row([[<<"PRI">>, COLUMN_NAME, _, undefined] | Format], Fill, Name) ->
    format_row(Format, [fun(A) -> integer_to_binary(atomics:add_get(persistent_term:get(A), 1, 1)) end | Fill], [COLUMN_NAME | Name]);

%% char type primary key
format_row([[<<"PRI">>, COLUMN_NAME, undefined, CHARACTER_MAXIMUM_LENGTH] | Format], Fill, Name) ->
    format_row(Format, [fun(A) -> I = atomics:add_get(persistent_term:get(A), 1, 1), <<(integer_to_binary(I))/binary, (binary:copy(<<"!">>, CHARACTER_MAXIMUM_LENGTH - trunc(math:log10(I)) - 1))/binary>> end | Fill], [COLUMN_NAME | Name]);

%% integer type unique key
format_row([[<<"UNI">>, COLUMN_NAME, _, undefined] | Format], Fill, Name) ->
    format_row(Format, [fun(A) -> integer_to_binary(atomics:add_get(persistent_term:get(A), 1, 1)) end | Fill], [COLUMN_NAME | Name]);

%% char type unique key
format_row([[<<"UNI">>, COLUMN_NAME, undefined, CHARACTER_MAXIMUM_LENGTH] | Format], Fill, Name) ->
    format_row(Format, [fun(A) -> I = atomics:add_get(persistent_term:get(A), 1, 1), <<(integer_to_binary(I))/binary, (binary:copy(<<"!">>, CHARACTER_MAXIMUM_LENGTH - trunc(math:log10(I)) - 1))/binary>> end | Fill], [COLUMN_NAME | Name]);

%% integer type
format_row([[_, COLUMN_NAME, _, undefined] | Format], Fill, Name) ->
    format_row(Format, [fun(_) -> <<"0">> end | Fill], [COLUMN_NAME | Name]);

%% char type
format_row([[_, COLUMN_NAME, undefined, CHARACTER_MAXIMUM_LENGTH] | Format], Fill, Name) ->
    format_row(Format, [fun(_) -> binary:copy(<<"!">>, CHARACTER_MAXIMUM_LENGTH) end | Fill], [COLUMN_NAME | Name]).

%%%===================================================================
%%% Tools
%%%===================================================================
%% join separator
join(List) ->
    join_loop(List, <<>>).

join_loop([], Binary) ->
    Binary;
join_loop([H], Binary) ->
    <<Binary/binary, H/binary>>;
join_loop([H | T], Binary) ->
    join_loop(T, <<Binary/binary, H/binary, ",">>).
