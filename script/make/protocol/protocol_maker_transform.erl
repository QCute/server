%%%-------------------------------------------------------------------
%%% @doc
%%% make protocol script transform
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_transform).
-export([parse_transform/2, parse_transform_info/0]).
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
    New = {record_field, RecordFieldRowColumn, {atom, AtomRowColumn, encode}, convert(Data, data, Comment)},
    convert_from_io_record(T, Comment, [New | List]);

convert_from_io_record([{record_field, RecordFieldRowColumn, {atom, AtomRowColumn, decode}, Data} | T], Comment, List) ->
    New = {record_field, RecordFieldRowColumn, {atom, AtomRowColumn, decode}, convert(Data, data, Comment)},
    convert_from_io_record(T, Comment, [New | List]);

convert_from_io_record([Other | T], Comment, List) ->
    convert_from_io_record(T, Comment, [Other | List]).


%% convert
convert({call, Line, CallName, Arguments}, Name, Comment) ->
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NewArgument = maps:get(Arguments, #{[] => [{atom, Line, undefined}]}, Arguments),
    {call, Line, CallName, [{atom, Line, Name} | NewArgument] ++ [{string, Line, ThisLineComment}]};

convert({tuple, Line, Form}, Name, Comment) ->
    Field = convert_from_tuple(Form, Comment, []),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    {call, Line, {atom, Line, tuple}, [{atom, Line, Name}, {tuple, Line, Field}, {string, Line, ThisLineComment}]};

convert({record, Line, Name, Form}, _, Comment) ->
    Field = convert_from_record(Form, Comment, []),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    {call, Line, {atom, Line, record}, [{atom, Line, Name}, {record, Line, Name, Field}, {string, Line, ThisLineComment}]};

convert({map, Line, Form}, Name, Comment) ->
    Field = convert_from_maps(Form, Comment, []),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    {call, Line, {atom, Line, maps}, [{atom, Line, Name}, {map, Line, Field}, {string, Line, ThisLineComment}]};

convert({cons, Line, {match, MatchLine, {cons, {atom, MatchNameLine, MatchName}, MatchNext}, Form}, Next}, Name, Comment) ->
    Field = convert(Form, "", Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Name, Comment),
    MatchNextField = convert(MatchNext, Name, Comment),
    {call, Line, {atom, Line, ets}, [{atom, Line, Name}, {cons, {atom, MatchNameLine, MatchName}, MatchNextField}, {cons, Line, {match, MatchLine, {var, MatchNameLine, '_'}, Field}, NextField}, {string, Line, ThisLineComment}]};

convert({cons, Line, {match, _, {nil, NilLine}, Form}, Next}, Name, Comment) ->
    Field = convert(Form, "", Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Name, Comment),
    {call, Line, {atom, Line, ets}, [{atom, Line, Name}, {nil, NilLine}, {cons, Line, Field, NextField}, {string, Line, ThisLineComment}]};

convert({cons, Line, {match, MatchLine, {atom, MatchNameLine, MatchName}, Form}, Next}, Name, Comment) ->
    Field = convert(Form, "", Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Name, Comment),
    {call, Line, {atom, Line, list}, [{atom, Line, Name}, {atom, Line, MatchName}, {cons, Line, {match, MatchLine, {var, MatchNameLine, '_'}, Field}, NextField}, {string, Line, ThisLineComment}]};

convert({cons, Line, Form, Next}, Name, Comment) ->
    Field = convert(Form, "", Comment),
    ThisLineComment = maps:get(element(1, Line), Comment, ""),
    NextField = convert(Next, Name, Comment),
    {call, Line, {atom, Line, list}, [{atom, Line, Name}, {atom, Line, undefined}, {cons, Line, Field, NextField}, {string, Line, ThisLineComment}]};

convert(Other, _, _) ->
    Other.


%% tuple type
convert_from_tuple([], _, List) ->
    lists:reverse(List);

convert_from_tuple([{match, MatchLine, {atom, MatchNameLine, AtomData}, Data} | T], Comment, List) ->
    New = {match, MatchLine, {var, MatchNameLine, '_'}, convert(Data, AtomData, Comment)},
    convert_from_tuple(T, Comment, [New | List]).


%% record type
convert_from_record([], _, List) ->
    lists:reverse(List);

convert_from_record([{record_field, RecordLine, {atom, AtomLine, Name}, Data} | T], Comment, List) ->
    New = {record_field, RecordLine, {atom, AtomLine, Name}, convert(Data, Name, Comment)},
    convert_from_record(T, Comment, [New | List]).


%% maps type
convert_from_maps([], _, List) ->
    lists:reverse(List);

convert_from_maps([{'map_field_assoc', KeyLine, {atom, AtomLine, Name}, Data} | T], Comment, List) ->
    New = {'map_field_assoc', KeyLine, {atom, AtomLine, Name}, convert(Data, Name, Comment)},
    convert_from_maps(T, Comment, [New | List]).

%%%===================================================================
%%% Transform info
%%%===================================================================
-spec parse_transform_info() -> #{'error_location' => 'column'}.
parse_transform_info() ->
    #{error_location => column}.
