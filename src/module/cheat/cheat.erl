%%%-------------------------------------------------------------------
%%% @doc
%%% cheat
%%% @end
%%%-------------------------------------------------------------------
-module(cheat).
%% API
-export([is_open/1]).
-export([query/1]).
-export([cheat/2]).
%% Includes
-include("../../../include/common.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/user.hrl").
-include("../../../include/role.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/item.hrl").
-include("../../../include/charge.hrl").
%% Macros
-ifdef(DEBUG).
-define(CHEAT, true).
-else.
-define(CHEAT, false).
-endif.
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc is open
-spec is_open(User :: #user{}) -> ok().
is_open(_) ->
    {ok, ?CHEAT}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(User) ->
    query(User, ?CHEAT).

-dialyzer({no_match, query/2}).
query(_, false) ->
    {ok, []};
query(_, true) ->
    %% from source is exists
    case file:read_file(beam:source(?MODULE)) of
        {ok, Data} ->
            %% last execute_command function
            [{Offset, _} | _] = lists:reverse(binary:matches(Data, <<"execute_command">>)),
            <<_:Offset/binary, Tail/binary>> = Data,
            %% extract description and command define
            {match, MatchList} = re:run(Tail, "(?m)(?s)(?<=@doc).*?(?=\\-\\>)", [global, {capture, all, binary}]),
            List = lists:map(fun([Preset]) ->
                [D, C] = binary:split(Preset, <<"\n">>),
                %% remove description space, remove command string quote " and list quote []
                NamesBlock = re:replace(D, "^\s*|\\[|\"|\\]|\s*$", "", [global, {return, binary}]),
                Names = binary:split(NamesBlock, <<",">>, [global, trim_all]),
                CommandsBlock = re:replace(C, "^\s*|\\[|\"|\\]|\s*$", "", [global, {return, binary}]),
                Commands = binary:split(CommandsBlock, <<",">>, [global, trim_all]),
                length(Names) =/= length(Commands) andalso length(Names) + 1 =/= length(Commands) andalso erlang:throw(lists:flatten(io_lib:format("cheat command define error: ~ts and ~ts", [D, C]))),
                [{Name, Command} | Args] = lists:zip(Names ++ [hd(Names) || length(Names) + 1 == length(Commands)], Commands),
                {Name, Command, Args}
            end, MatchList),

            %% make cheat list beam
            %% make list form
            ListForm = parser:to_form(List),
            %% make module form
            Forms = [
                {attribute, 1, file, {"cheat_list", 1}},
                {attribute, 1, module, cheat_list},
                {attribute, 2, export, [{list, 0}]},
                {function, 1, list, 0, [{clause, 1, [], [], [ListForm]}]},
                {eof, 1}
            ],
            {ok, Module, Binary} = compile:forms(Forms),
            file:write_file(lists:concat([config:path_beam(), "/cheat_list.beam"]), Binary),
            code:load_binary(Module, "cheat_list", Binary),
            {ok, List};
        _ ->
            %% from beam if exists
            case erlang:function_exported(cheat_list, list, 0) of
                true ->
                    {ok, erlang:apply(cheat_list, list, [])};
                false ->
                    {ok, []}
            end
    end.

-dialyzer({no_match, cheat/2}).
%% @doc cheat
-spec cheat(User :: #user{}, Command :: binary()) -> ok() | error().
cheat(User, Command) ->
    case execute_command(User, lists:reverse(Command), ?CHEAT) of
        {ok, NewUser = #user{}} ->
            {ok, ok, NewUser};
        Error ->
            Error
    end.

-dialyzer({no_match, execute_command/3}).
%% execute command
execute_command(_User, _Command, false) ->
    ok;
execute_command(User = #user{role_id = RoleId, role_name = RoleName}, Command, true) ->
    case [string:strip(unicode:characters_to_list(String)) || String <- Command] of
        %% @doc 登出
        ["logout"] ->
            gen_server:cast(self(), {stop, normal});
        %% @doc 充值, 充值Id
        ["charge", ChargeId] ->
            #charge_data{now_price = NowPrice} = charge_data:get(type:to_integer(ChargeId)),
            Charge = #charge_order{
                charge_id = type:to_integer(ChargeId),
                order_id = type:to_binary(time:millisecond()),
                channel = type:to_binary(?MODULE),
                role_id = RoleId,
                role_name = RoleName,
                money = NowPrice,
                time = time:now()
            },
            ChargeNo = charge_order_sql:insert(Charge),
            charge:callback(User, ChargeNo);
        %% @doc 等级
        ["level", Level] ->
            case level_data:exp(type:to_integer(Level)) - User#user.asset#asset.exp of
                0 ->
                    {ok, User};
                Number when Number > 0 ->
                    asset:add(User, [{exp, Number}], ?MODULE);
                Number ->
                    asset:cost(User, [{exp, -Number}], ?MODULE)
            end;
        %% @doc 职业
        ["classes", Classes] ->
            role:change_classes(User, type:to_integer(Classes));
        %% @doc 钻石
        ["diamond", Value] ->
            asset:add(User, [{diamond, type:to_integer(Value)}], ?MODULE);
        %% @doc 金币
        ["gold", Value] ->
            asset:add(User, [{gold, type:to_integer(Value)}], ?MODULE);
        %% @doc 银币
        ["silver", Value] ->
            asset:add(User, [{silver, type:to_integer(Value)}], ?MODULE);
        %% @doc 铜币
        ["copper", Value] ->
            asset:add(User, [{copper, type:to_integer(Value)}], ?MODULE);
        %% @doc 硬币
        ["coin", Value] ->
            asset:add(User, [{coin, type:to_integer(Value)}], ?MODULE);
        %% @doc 经验
        ["exp", Value] ->
            asset:add(User, [{exp, type:to_integer(Value)}], ?MODULE);
        %% @doc 物品, 物品Id, 数量
        ["item", ItemId, Number] ->
            case item_data:get(type:to_integer(ItemId)) of
                #item_data{} ->
                    item:add(User, [{type:to_integer(ItemId), type:to_integer(Number)}], ?MODULE);
                _ ->
                    {error, item_not_found}
            end;
        _ ->
            {error, cheat_command_not_found}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
