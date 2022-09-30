%%%-------------------------------------------------------------------
%%% @doc
%%% make user loop(load/save/reset/clean/expire/login/logout/reconnect/disconnect) code
%%% @end
%%%-------------------------------------------------------------------
-module(loop_maker).
-export([start/1]).
-export([make_template/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_file({OutFile, InFile, [Name | Args]}) ->
    ArgList = maker:parse_args(Args),
    %% add user field
    List = [{"load", "load"}, {"save", "save"}, {"reset", "reset"}, {"clean", "clean"}, {"expire", "expire"}, {"login", "login"}, {"logout", "logout"}, {"reconnect", "reconnect"}, {"disconnect", "disconnect"}],
    Comment = io_lib:format("%% ~ts (~s)", [unicode:characters_to_binary(proplists:get_value("comment", ArgList, Name)), string:join([Value || {Arg, Value} <- List, proplists:is_defined(Arg, ArgList)], "/")]),
    %% field position
    FieldList = beam:find(user),
    Default = lists:nth(listing:index(role_id, FieldList), FieldList),
    After = type:to_atom(hd(proplists:get_value("after", ArgList, [Default]))),
    not lists:member(After, FieldList) andalso erlang:throw(lists:flatten(io_lib:format("could not found ~s in user", [After]))),
    {ok, Binary} = file:read_file(maker:relative_path(InFile)),
    [Head, Rest] = re:split(Binary, lists:flatten(io_lib:format("\n(?=\\s*~s)", [After]))),
    {Start, _} = binary:match(Rest, <<"\n">>),
    <<This:Start/binary, $\n, Tail/binary>> = Rest,
    %% insert field
    Insert = unicode:characters_to_binary(lists:concat(["\n    ", Name, " = [],", string:join(lists:duplicate(50 - length(Name) - 6, " "), ""), Comment, "\n"])),
    file:write_file(maker:relative_path(InFile), <<Head/binary, $\n, This/binary, Insert/binary, Tail/binary>>),
    %% make module template
    TemplateFile = maker:relative_path(lists:concat(["src/module/", Name, "/", Name, ".erl"])),
    Result = not filelib:is_regular(TemplateFile) andalso make_template(TemplateFile, Name, proplists:get_value("comment", ArgList, "")),
    Result =/= ok andalso erlang:throw(Result),
    parse_file({OutFile, InFile, []});
parse_file({_, InFile, _}) ->
    Result = analyse(InFile),
    %% only loop store data field
    Position = listing:index(role_id, beam:find(user)) - 1,
    [{"(?<=-define\\(END_POSITION,)\\s*\\d+(?=\\)\\.)", lists:concat([" ", integer_to_list(Position)])} | make_code(Result, [])].

%% analyse file code
analyse(File) ->
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    {match, [String]} = re:run(Binary, "(?m)(?s)^-record\\(user\\s*,\\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\\s|%)", [{capture, first, list}]),
    List = string:tokens(String, "\n"),
    TypeList = [
        {"load", [], []},
        {"save", [], []},
        {"reset", [], []},
        {"clean", [], []},
        {"expire", [], []},
        {"login", [], []},
        {"logout", [], []},
        {"reconnect", [], []},
        {"disconnect", [], []}
    ],
    analyse_row(List, TypeList).

analyse_row([], List) ->
    lists:reverse(List);
analyse_row([Row | T], List) ->
    case re:run(Row, "%%.*?\\(.*?\\)", [{capture, all, list}]) of
        {match, [String]} ->
            Expression = hd(string:tokens(Row, "%%")),
            Assignment = hd(string:tokens(Expression, "=")),
            Name = [X || X <- Assignment, X =/= $, andalso X =/= 32],
            Index = integer_to_list(listing:index(list_to_atom(Name), beam:find(user))),
            TypeList = make_Type(List, Index, Name, String, []),
            analyse_row(T, TypeList);
        _ ->
            analyse_row(T, List)
    end.

%% make loop type
make_Type([], _, _, _, List) ->
    List;
make_Type([{Type, IndexList, NameList} | T], Index, Name, String, List) ->
    case string:str(String, Type) =/= 0 of
        true ->
            make_Type(T, Index, Name, String, [{Type, [Index | IndexList], [Name | NameList]} | List]);
        false ->
            make_Type(T, Index, Name, String, [{Type, IndexList, NameList} | List])
    end.

%% make all field code
make_code([], List) ->
    List;
make_code([{Type, IndexList, NameList} | T], List) ->
    IndexPattern = {format_index_math(Type), format_index(lists:reverse(IndexList))},
    CodePattern = {format_code_match(Type), string:join([format_code(Type, Name) || Name <- lists:reverse(NameList)], "") ++ format_end_code(Type)},
    make_code(T, [IndexPattern, CodePattern | List]).

%% index define match
format_index_math(Type) ->
    lists:flatten(io_lib:format("(?<=-define\\(~s_LIST,)\\s*.*?(?=\\)\\.)", [string:to_upper(Type)])).

%% index define code
format_index(IndexList) ->
    lists:flatten(io_lib:format(" [~ts]", [string:join(IndexList, ", ")])).

%% code match
format_code_match(Type) ->
    lists:flatten(io_lib:format("(?m)(?s)(?<!\\S)(^do_~s.+?)(?=\\.$|\\%)\\.\\n?\\n?", [Type])).

%% code
format_code(Type, Name) ->
    lists:flatten(io_lib:format("do_~s(#user.~s, User) ->\n    ~s:~s(User);~n", [Type, Name, Name, Type])).

%% end code
format_end_code(Type) ->
    lists:flatten(io_lib:format("do_~s(_, User) ->\n    User.\n\n", [Type])).

%% make user module template
make_template(File, Name, Comment) ->
    HumpName = word:to_hump(Name),
    Data = io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% ~s ~ts
%%% @end
%%%-------------------------------------------------------------------
-module(~s).
%% API
-export([load/1, save/1]).
-export([query/1]).
%% Includes
-include(\"common.hrl\").
-include(\"user.hrl\").
-include(\"~s.hrl\").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    ~s = ~s_sql:select(RoleId),
    User#user{~s = ~s}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{~s = ~s}) ->
    New~s = ~s_sql:insert_update(~s),
    User#user{~s = New~s}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{~s = ~s}) ->
    {ok, ~s}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
",  [Name, Comment, Name, Name, HumpName, Name, Name, HumpName, Name, HumpName, HumpName, Name, HumpName, Name, HumpName, Name, HumpName, HumpName]),
    filelib:ensure_dir(File),
    file:write_file(File, unicode:characters_to_binary(Data)).
