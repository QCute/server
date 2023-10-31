%%%-------------------------------------------------------------------
%%% @doc
%%% make user loop(create/load/save/reset/clean/expire/login/logout/reconnect/disconnect) code
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
parse_file(#{file := File, header := Header, args := [Name | Args]}) ->
    ArgList = maker:parse_args(Args),
    %% add user field
    List = [
        {"create", "create"},
        {"load", "load"},
        {"save", "save"},
        {"reset", "reset"},
        {"clean", "clean"},
        {"expire", "expire"},
        {"login", "login"},
        {"logout", "logout"},
        {"reconnect", "reconnect"},
        {"disconnect", "disconnect"}
    ],
    Comment = io_lib:format("%% ~ts (~s)", [unicode:characters_to_binary(proplists:get_value("comment", ArgList, Name)), string:join([Value || {Arg, Value} <- List, proplists:is_defined(Arg, ArgList)], "/")]),
    %% field position
    FieldList = beam:find(user),
    Default = lists:nth(listing:index(role_id, FieldList), FieldList),
    After = type:to_atom(hd(proplists:get_value("after", ArgList, [Default]))),
    not lists:member(After, FieldList) andalso erlang:throw(lists:flatten(io_lib:format("could not found ~s in user", [After]))),
    {ok, Binary} = file:read_file(maker:relative_path(Header)),
    [Head, Rest] = re:split(Binary, lists:flatten(io_lib:format("\n(?=\\s*~s)", [After]))),
    {Start, _} = binary:match(Rest, <<"\n">>),
    <<This:Start/binary, $\n, Tail/binary>> = Rest,
    %% insert field
    Insert = unicode:characters_to_binary(lists:concat(["\n    ", Name, " = [],", string:join(lists:duplicate(50 - length(Name) - 6, " "), ""), Comment, "\n"])),
    file:write_file(maker:relative_path(Header), <<Head/binary, $\n, This/binary, Insert/binary, Tail/binary>>),
    %% make module template
    TemplateFile = maker:relative_path(lists:concat(["src/module/", Name, "/", Name, ".erl"])),
    Result = not filelib:is_regular(TemplateFile) andalso make_template(TemplateFile, Name, proplists:get_value("comment", ArgList, "")),
    Result =/= ok andalso erlang:throw(Result),
    parse_file(#{file => File, header => Header, args => []});
parse_file(#{file := File, header := Header}) ->
    %% take loop type
    [_, _, Type] = string:split(filename:basename(File, ".erl"), "_", all),
    {ok, Binary} = file:read_file(maker:relative_path(Header)),
    {match, [String]} = re:run(Binary, "(?m)(?s)^-record\\(user\\s*,\\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\\s|%)", [{capture, first, list}]),
    List = string:tokens(String, "\n"),
    %% analyse file code
    analyse_row(List, Type, [], []).

analyse_row([], Type, IndexList, NameList) ->
    IndexListCode = string:join(lists:reverse(IndexList), ", "),
    DoCode = string:join([io_lib:format("do_~s(#user.~s, User) ->\n    ~s:~s(User);", [Type, Name, Name, Type]) || Name <- lists:reverse(NameList)], "\n"),
    Code = io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% user ~s loop
%%% @end
%%%-------------------------------------------------------------------
-module(user_loop_~s).
%% API
-export([loop/1, loop_list/2, loop_range/3]).
%% Includes
-include(\"user.hrl\").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc ~s loop
-spec loop(User :: #user{}) -> NewUser :: #user{}.
loop(User) ->
    loop_list([~s], User).

%% @doc ~s loop list
-spec loop_list(List :: [pos_integer()], User :: #user{}) -> NewUser :: #user{}.
loop_list([], User) ->
    User;
loop_list([Position | T], User) ->
    loop_list(T, do_~s(Position, User)).

%% @doc ~s loop range
-spec loop_range(Position :: pos_integer(), Size :: non_neg_integer(), User :: #user{}) -> NewUser :: #user{}.
loop_range(Size, Size, User) ->
    do_~s(Size, User);
loop_range(Position, Size, User) ->
    loop_range(Position + 1, Size, do_~s(Position, User)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
~s
do_~s(_, User) ->
    User.
", [Type, Type, Type, IndexListCode, Type, Type, Type, Type, Type, DoCode, Type]),
    [#{pattern => "(?s).*", code => Code}];
analyse_row([Row | T], Type, IndexList, NameList) ->
    case re:run(Row, "%%.*?\\(.*?\\)", [{capture, all, list}]) of
        {match, [String]} ->
            Expression = hd(string:tokens(Row, "%%")),
            Assignment = hd(string:tokens(Expression, "=")),
            Name = [X || X <- Assignment, X =/= $, andalso X =/= 32],
            Index = integer_to_list(listing:index(list_to_atom(Name), beam:find(user))),
            case string:str(String, Type) =/= 0 of
                true ->
                    analyse_row(T, Type, [Index | IndexList], [Name | NameList]);
                false ->
                    analyse_row(T, Type, IndexList, NameList)
            end;
        _ ->
            analyse_row(T, Type, IndexList, NameList)
    end.

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
