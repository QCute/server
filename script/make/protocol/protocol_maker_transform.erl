%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol script transform
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_transform).
-export([parse_transform/2, parse_transform_info/0]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% Transform
%%%===================================================================
-spec parse_transform(Forms, Options) -> Forms2 | Errors | Warnings when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Forms2 :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Options :: term(),
    Errors :: {error, ErrInfo :: [tuple()], WarnInfo :: []},
    Warnings :: {warning, Forms2, WarnInfo :: [tuple()]}.
parse_transform(Forms = [{attribute, _, file, {File, _}} | _], _Options_) ->
    {ok, Binary} = file:read_file(File),
    %% @attention e script mode will remove first shebang line
    {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Binary), 0, [return_comments]),
    Comment = maps:from_list([{Line, lists:flatten(string:trim(Data))} || {comment, Line, [_, _ | Data]} <- Tokens]),
    convert_from_function(Forms, Comment, []).


%% protocol() -> #protocol{ ... }.
convert_from_function([], _, List) ->
    lists:reverse(List);

convert_from_function([{function, FunctionRowColumn, protocol, Argument, [{clause, ClauseRowColumn, [], [], [{record, RecordRowColumn, protocol, RecordField}]}]} | T], Comment, List) ->
    New = {function, FunctionRowColumn, protocol, Argument, [{clause, ClauseRowColumn, [], [], [{record, RecordRowColumn, protocol, convert_from_protocol_record(RecordField, Comment, [])}]}]},
    convert_from_function(T, Comment, [New | List]);

convert_from_function([Other | T], Comment, List) ->
    convert_from_function(T, Comment, [Other | List]).


%% #protocol{io = ...}
convert_from_protocol_record([], _, List) ->
    lists:reverse(List);

convert_from_protocol_record([{record_field, RecordFieldRowColumn, {atom, AtomRowColumn, io}, Data} | T], Comment, List) ->
    New = {record_field, RecordFieldRowColumn, {atom, AtomRowColumn, io}, convert_from_io_list(Data, Comment)},
    convert_from_protocol_record(T, Comment, [New | List]);

convert_from_protocol_record([Other | T], Comment, List) ->
    convert_from_protocol_record(T, Comment, [Other | List]).


%% io = [...]
convert_from_io_list({cons, Line, {record, RecordLineColumn, io, RecordField}, {nil, NilLine}}, Comment) ->
    New = convert_from_io_record(RecordField, Comment, []),
    {cons, Line, {record, RecordLineColumn, io, New}, {nil, NilLine}};

convert_from_io_list({cons, Line, {record, RecordLineColumn, io, RecordField}, Next}, Comment) ->
    New = convert_from_io_record(RecordField, Comment, []),
    NextNew = convert_from_io_list(Next, Comment),
    {cons, Line, {record, RecordLineColumn, io, New}, NextNew}.


%% encode = .../decode = ...
convert_from_io_record([], _, List) ->
    lists:reverse(List);

convert_from_io_record([{record_field, RecordFieldRowColumn, {atom, AtomRowColumn, encode}, Data} | T], Comment, List) ->
    New = {record_field, RecordFieldRowColumn, {atom, AtomRowColumn, encode}, convert(Data, encode, root, undefined, [], Comment)},
    convert_from_io_record(T, Comment, [New | List]);

convert_from_io_record([{record_field, RecordFieldRowColumn, {atom, AtomRowColumn, decode}, Data} | T], Comment, List) ->
    New = {record_field, RecordFieldRowColumn, {atom, AtomRowColumn, decode}, convert(Data, decode, root, undefined, [], Comment)},
    convert_from_io_record(T, Comment, [New | List]);

convert_from_io_record([Other | T], Comment, List) ->
    convert_from_io_record(T, Comment, [Other | List]).


%% convert
convert({call, Line, CallName, Arguments}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NewArgument = maps:get(Arguments, #{[] => [{atom, Line, undefined}]}, Arguments),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, CallName, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get | NewArgument] ++ [{string, Line, ThisLineComment}]};

convert({tuple, Line, This}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    Field = convert_from_tuple(This, Mode, tuple, NewAncestor, Comment, []),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$tuple$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {tuple, Line, Field}, {string, Line, ThisLineComment}]};

convert({record, Line, Tag, This}, Mode, From, OriginName, Ancestor, Comment) ->
    %% name undefined use record `tag` as name
    case OriginName of
        undefined ->
            %% @todo maybe change name to tag name
            %% Name = Tag;
            Name = OriginName;
        _ ->
            Name = OriginName
    end,
    NewAncestor = [Name | Ancestor],
    Field = convert_from_record(This, Mode, record, NewAncestor, Comment, []),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$record$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {atom, Line, Tag}, {record, Line, Tag, Field}, {string, Line, ThisLineComment}]};

convert({map, Line, This}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    Field = convert_from_maps(This, Mode, map, NewAncestor, Comment, []),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$maps$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {map, Line, Field}, {string, Line, ThisLineComment}]};

convert({cons, Line, {match, MatchLine, {cons, {atom, MatchNameLine, MatchName}, MatchNext}, This}, Next}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    Field = convert(This, Mode, ets, undefined, NewAncestor, Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Mode, From, Name, NewAncestor, Comment),
    MatchNextField = convert(MatchNext, Mode, From, Name, NewAncestor, Comment),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$ets$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {cons, {atom, MatchNameLine, MatchName}, MatchNextField}, {cons, Line, {match, MatchLine, {var, MatchNameLine, '_'}, Field}, NextField}, {string, Line, ThisLineComment}]};

convert({cons, Line, {match, _, {nil, NilLine}, This}, Next}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    Field = convert(This, Mode, ets, undefined, NewAncestor, Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Mode, list, Name, NewAncestor, Comment),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$ets$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {nil, NilLine}, {cons, Line, Field, NextField}, {string, Line, ThisLineComment}]};

convert({cons, Line, {match, MatchLine, {atom, MatchNameLine, MatchName}, This}, Next}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    Field = convert(This, Mode, list, undefined, NewAncestor, Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Mode, list, Name, NewAncestor, Comment),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$list$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {atom, Line, MatchName}, {cons, Line, {match, MatchLine, {var, MatchNameLine, '_'}, Field}, NextField}, {string, Line, ThisLineComment}]};

convert({cons, Line, This, Next}, Mode, From, Name, Ancestor, Comment) ->
    NewAncestor = [Name | Ancestor],
    Field = convert(This, Mode, list, undefined, NewAncestor, Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Mode, list, Name, NewAncestor, Comment),
    Path = make_path(NewAncestor, Mode, From, Line, {nil, Line}),
    Prefix = make_prefix(NewAncestor, Mode, From, Line, []),
    Suffix = make_suffix(NewAncestor, Mode, From, Line, []),
    Get = make_get(NewAncestor, Mode, From, Line, []),
    {call, Line, {atom, Line, '$list$'}, [{atom, Line, convert_name(Name)}, {atom, Line, From}, Path, Prefix, Suffix, Get, {atom, Line, undefined}, {cons, Line, Field, NextField}, {string, Line, ThisLineComment}]};

convert(Other, _, _, _, _, _) ->
    Other.


%% tuple type
convert_from_tuple([], _, _, _, _, List) ->
    lists:reverse(List);

convert_from_tuple([{match, MatchLine, {atom, MatchNameLine, AtomData}, Data} | T], Mode, From, Ancestor, Comment, List) ->
    New = {match, MatchLine, {var, MatchNameLine, '_'}, convert(Data, Mode, From, AtomData, Ancestor, Comment)},
    convert_from_tuple(T, Mode, From, Ancestor, Comment, [New | List]);

convert_from_tuple([{_, {MatchLine, MatchLineColumn}, _, _} | _], _, _, _, _, _) ->
    erlang:throw(lists:flatten(io_lib:format("Tuple field name can not be empty: ~tw:~tw", [MatchLine, MatchLineColumn]))).


%% record type
convert_from_record([], _, _, _, _, List) ->
    lists:reverse(List);

convert_from_record([{record_field, RecordLine, {atom, AtomLine, Name}, Data} | T], Mode, From, Ancestor, Comment, List) ->
    New = {record_field, RecordLine, {atom, AtomLine, Name}, convert(Data, Mode, From, Name, Ancestor, Comment)},
    convert_from_record(T, Mode, From, Ancestor, Comment, [New | List]).


%% maps type
convert_from_maps([], _, _, _, _, List) ->
    lists:reverse(List);

convert_from_maps([{'map_field_assoc', KeyLine, {atom, AtomLine, Name}, Data} | T], Mode, From, Ancestor, Comment, List) ->
    New = {'map_field_assoc', KeyLine, {atom, AtomLine, Name}, convert(Data, Mode, From, Name, Ancestor, Comment)},
    convert_from_maps(T, Mode, From, Ancestor, Comment, [New | List]).


%% convert name
convert_name(undefined) ->
    ?ANONYMOUS_DATA_NAME;
convert_name(Name) ->
    Name.


%% make list cons
make_path([], _, _, _, Cons) ->
    Cons;
make_path([undefined | T], Mode = encode, From, Line, Cons) ->
    make_path(T, Mode, From, Line, {cons, Line, {atom, Line, ?ANONYMOUS_DATA_NAME}, Cons});
make_path([undefined | T], Mode = decode, From, Line, Cons) ->
    make_path(T, Mode, From, Line, {cons, Line, {atom, Line, ?ANONYMOUS_DATA_NAME}, Cons});
make_path([undefined | T], Mode, From, Line, Cons) ->
    make_path(T, Mode, From, Line, {cons, Line, {atom, Line, ?ANONYMOUS_DATA_NAME}, Cons});
make_path([H | T], Mode, From, Line, Cons) ->
    make_path(T, Mode, From, Line, {cons, Line, {atom, Line, H}, Cons}).


%% make prefix list cons
make_prefix([], Mode, From, Line, Cons) ->
    make_path(Cons, Mode, From, Line, {nil, Line});
make_prefix([undefined | T], Mode, From, Line, _) ->
    make_path([undefined | T], Mode, From, Line, {nil, Line});
make_prefix([H | T], Mode, From, Line, Cons) ->
    make_prefix(T, Mode, From, Line, [H | Cons]).


%% make suffix list cons
make_suffix([], Mode, From, Line, Cons = [_ | _]) ->
    %% remove last
    make_path(tl(lists:reverse(Cons)), Mode, From, Line, {nil, Line});
make_suffix([], Mode, From, Line, Cons) ->
    make_path(lists:reverse(Cons), Mode, From, Line, {nil, Line});
make_suffix([undefined | _], Mode, From, Line, Cons = [_ | _]) ->
    %% remove last
    make_path(tl(lists:reverse(Cons)), Mode, From, Line, {nil, Line});
make_suffix([undefined | _], Mode, From, Line, Cons) ->
    make_path(lists:reverse(Cons), Mode, From, Line, {nil, Line});
make_suffix([H | T], Mode, From, Line, Cons) ->
    make_suffix(T, Mode, From, Line, [H | Cons]).


%% make set list cons
make_get([], Mode, From, Line, Cons) ->
    make_path(lists:reverse(Cons), Mode, From, Line, {nil, Line});
make_get([undefined | _], Mode, From, Line, Cons) ->
    make_path(lists:reverse(Cons), Mode, From, Line, {nil, Line});
make_get([H | _], Mode, From, Line, _) ->
    %% only last
    make_path([H], Mode, From, Line, {nil, Line}).

%%%===================================================================
%%% Transform info
%%%===================================================================
-spec parse_transform_info() -> #{'error_location' => 'column'}.
parse_transform_info() ->
    #{error_location => column}.