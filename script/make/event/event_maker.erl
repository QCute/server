%%%-------------------------------------------------------------------
%%% @doc
%%% make event dispatch
%%% @end
%%%-------------------------------------------------------------------
-module(event_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_file(#{include := Include, state := State, name := Name, wildcard := Wildcard}) ->
    List = parse_file_loop(filelib:wildcard(Wildcard), Include, State, Name, []),
    Code = format_code(List, State, Name, []),
    [#{pattern => "(?m)(?s)^trigger.*?\\.$", code => Code}].

%% parse file [{function, [{module, function, parameter, return}, ...]}, ...]
parse_file_loop([], _, _, _, List) ->
    listing:key_group(2, lists:append(List));
parse_file_loop([File | T], Include, State, Name, List) ->
    {ok, Data} = file:read_file(File),
    case binary:match(Data, <<"on_">>) of
        {_, _} ->
            %% epp
            {ok, Forms} = epp:parse_file(File, [Include], []),
            %% take handler {module, function, event, return}
            Form = parse_form_loop(Forms, list_to_atom(filename:basename(File, ".erl")), State, Name, []),
            parse_file_loop(T, Include, State, Name, [Form | List]);
        _ ->
            parse_file_loop(T, Include, State, Name, List)
    end.

%% parse form [{module, function, parameter, return}, ...]
parse_form_loop([], _, _, _, List) ->
    List;
parse_form_loop([{attribute, _, spec, {{Function, _}, [{type, _, 'fun', [{type, _, product, Parameter = [{ann_type, _, [{var, _, _}, {type, _, record, [{atom, _, State}]}]} | _]}, ReturnType]}]}} | T], Module, State, Name, List) ->
    case string:tokens(atom_to_list(Function), "_") of
        ["on" | _] ->
            parse_form_loop(T, Module, State, Name, [{Module, Function, [parse_type(Type, State, Name) || Type <- Parameter], parse_return_type(ReturnType, State, Name)} | List]);
        _ ->
            parse_form_loop(T, Module, State, Name, List)
    end;
parse_form_loop([{attribute, _, spec, {{Function, _}, [{type, _, 'fun', [{type, _, product, Parameter = [{type, _, record, [{atom, _, State}]} | _]}, ReturnType]}]}} | T], Module, State, Name, List) ->
    case string:tokens(atom_to_list(Function), "_") of
        ["on" | _] ->
            parse_form_loop(T, Module, State, Name, [{Module, Function, [parse_type(Type, State, Name) || Type <- Parameter], parse_return_type(ReturnType, State, Name)} | List]);
        _ ->
            parse_form_loop(T, Module, State, Name, List)
    end;
parse_form_loop([_ | T], Module, State, Name, List) ->
    parse_form_loop(T, Module, State, Name, List).

%% User :: #user{}
parse_type({ann_type, _, [{var, _, _}, {type, _, record, [{atom, _, State}]}]}, State, _) ->
    State;
%% #user{}
parse_type({type, _, record, [{atom, _, State}]}, State, _) ->
    State;
%% Event :: #event{}
parse_type({ann_type, _, [{var, _, _}, {type, _, record, [{atom, _, Event}]}]}, _, Event) ->
    Event;
%% #event{}
parse_type({type, _, record, [{atom, _, Event}]}, _, Event) ->
    Event;
%% non above all
parse_type(_, _, _) ->
    undefined.

%% User :: #user{}
parse_return_type({ann_type, _, [{var, _, _}, {type, _, record, [{atom, _, State}]}]}, State, _) ->
    State;
%% #user{}
parse_return_type({type, _, record, [{atom, _, State}]}, State, _) ->
    State;
%% Result :: ok
parse_return_type({ann_type, _, [{var, _, _}, {atom, _, ok}]}, _, _) ->
    ok;
%% ok
parse_return_type({atom, _, ok}, _, _) ->
    ok;
%% non above all
parse_return_type(_, _, _) ->
    undefined.

%% format code
format_code([], State, _, List) ->
    list_to_binary(lists:concat([[lists:concat([Code, ";\n"]) || Code <- List], "trigger(", word:to_hump(State), ", _) ->\n    ", word:to_hump(State), "."]));
format_code([{Function, CodeList} | T], State, Name, List) ->
    Expressions = format_code_list(CodeList, '', State, Name, []),
    %% remove on
    Event = lists:flatten(string:replace(atom_to_list(Function), "on_", "")),
    %% function clause
    EventName = [lists:concat([word:to_hump(Name), " = "]) || lists:all(fun({_, _, P, _}) -> lists:member(Name, P) end, CodeList)],
    Code = io_lib:format("trigger(~s, ~s#~s{name = ~s}) ->\n    ~s", [word:to_hump(State), EventName, Name, Event, Expressions]),
    format_code(T, State, Name, [Code | List]).

format_code_list([], Next, State, _, List) ->
    string:join(lists:reverse([lists:concat([word:to_hump(Next), word:to_hump(State)]) | List]), ",\n    ");

%% last return state
format_code_list([{Module, Function, Parameter, State}], Next, State, _, List) ->
    %% do not add assign and return
    Code = lists:concat([Module, ":", Function, "(", word:to_hump(Next), string:join([word:to_hump(atom_to_list(P)) || P <- Parameter], ", "), ")"]),
    string:join(lists:reverse([Code | List]), ",\n    ");

format_code_list([{Module, Function, Parameter, State} | T], Next, State, Name, List) ->
    Code = lists:concat([word:to_hump(Module), word:to_hump(State), " = ", Module, ":", Function, "(", word:to_hump(Next), string:join([word:to_hump(atom_to_list(P)) || P <- Parameter], ", "), ")"]),
    format_code_list(T, Module, State, Name, [Code | List]);

%% last return ok
format_code_list([{Module, Function, Parameter, ok}], Next, State, _, List) ->
    Code = lists:concat([Module, ":", Function, "(", word:to_hump(Next), "", string:join([word:to_hump(atom_to_list(P)) || P <- Parameter], ", "), ")"]),
    %% use pre state return
    LastReturn = lists:concat([Next, word:to_hump(State)]),
    string:join(lists:reverse([LastReturn, Code | List]), ",\n    ");

format_code_list([{Module, Function, Parameter, ok} | T], Next, State, Name, List) ->
    Code = lists:concat([Module, ":", Function, "(", word:to_hump(Next), "", string:join([word:to_hump(atom_to_list(P)) || P <- Parameter], ", "), ")"]),
    format_code_list(T, Next, State, Name, [Code | List]).
