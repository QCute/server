%%%------------------------------------------------------------------
%%% @doc
%%% module user load/save/clean maker
%%% @end
%%%------------------------------------------------------------------
-module(loop_maker).
-export([start/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/2, List).

%%%==================================================================
%%% Internal functions
%%%==================================================================
parse_file(_, {_, InFile}) ->
    Result = analyse(InFile),
    %% only loop store data field
    Position = listing:index(role_id, beam:find(user)) - 1,
    [{"(?<=-define\\(END_POSITION,)\\s*\\d+(?=\\)\\.)", integer_to_list(Position)}] ++ make_code(Result, []).

%% analyse file code
analyse(File) ->
    {ok, Binary} = file:read_file(maker:root_path() ++ File),
    {match, [String]} = re:run(Binary, "(?m)(?s)^-record\\(user\\s*,\\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\\s|%)", [{capture, first, list}]),
    List = string:tokens(String, "\n"),
    analyse_row(List, []).

analyse_row([], List) ->
    lists:reverse(List);
analyse_row([Row | T], List) ->
    case re:run(Row, "\\(.*?\\)", [{capture, all, list}]) of
        {match, [String]} ->
            Expression = hd(string:tokens(Row, "%%")),
            Assignment = hd(string:tokens(Expression, "=")),
            Name = [X || X <- Assignment, X =/= $, andalso X =/= 32],
            TypeList = make_type(String),
            Index = listing:index(list_to_atom(Name), beam:find(user)),
            analyse_row(T, [{Name, Index, TypeList} | List]);
        _ ->
            analyse_row(T, List)
    end.

%% make loop type
make_type(String) ->
    %% @hey add loop type here
    List = ["load", "save", "reset", "clean", "expire"],
    make_type(List, String, []).
make_type([], _, List) ->
    List;
make_type([Type | T], String, List) ->
    make_type(T, String, [{string:str(String, Type) =/= 0, Type} | List]).

%% make all field code
make_code([], List) ->
    lists:foldl(fun({Type, Code, IndexList}, Acc) -> [{format_code_match(Type), Code ++ format_end_code(Type)}, {format_index_math(Type), format_index(IndexList)} | Acc] end, [], List);
make_code([{Name, Index, TypeList} | T], List) ->
    NewList = make_code_loop(TypeList, Name, Index, List),
    make_code(T, NewList).

%% make every type code
make_code_loop([], _, _, List) ->
    List;
make_code_loop([{true, Type} | T], Name, Index, List) ->
    {_, Code, IndexList} = listing:key_find(Type, 1, List, {Type, [], []}),
    NewCode = Code ++ format_code(Type, Name),
    make_code_loop(T, Name, Index, [{Type, NewCode, [Index | IndexList]} | List]);
make_code_loop([_ | T], Name, Index, List) ->
    make_code_loop(T, Name, Index, List).

%% code match
format_code_match(Type) ->
    lists:flatten(io_lib:format("(?m)(?s)(?<!\\S)(^do_~s.+?)(?=\\.$|\\%)\\.\\n?\\n?", [Type])).

%% code
format_code(Type, Name) ->
    lists:flatten(io_lib:format("do_~s(#user.~s, User) ->\n    ~s:~s(User);~n", [Type, Name, Name, Type])).

%% end code
format_end_code(Type) ->
    lists:flatten(io_lib:format("do_~s(_, User) ->\n    User.\n\n", [Type])).

%% index define match
format_index_math(Type) ->
    lists:flatten(io_lib:format("(?<=-define\\(~s_LIST,)\\s*.*?(?=\\)\\.)", [string:to_upper(Type)])).

%% index define code
format_index(IndexList) ->
    lists:flatten(io_lib:format("~w", [lists:reverse(IndexList)])).
