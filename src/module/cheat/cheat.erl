%%%-------------------------------------------------------------------
%%% @doc
%%% cheat
%%% @end
%%%-------------------------------------------------------------------
-module(cheat).
%% API
-export([query/1]).
-export([cheat/2]).
%% Includes
-include("../../../include/common.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/user.hrl").
-include("../../../include/role.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/recharge.hrl").
%% Macros
-ifdef(DEBUG).
-define(CHEAT, 1).
-else.
-define(CHEAT, 0).
-endif.
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc query
-spec query(User :: #user{}) -> ok.
query(#user{sender_pid = SenderPid}) ->
    spawn(fun() -> reload(SenderPid, ?CHEAT) end),
    ok.

%% reload and extract
reload(_, 0) ->
    ok;
reload(SenderPid, 1) ->
    %% reload module
    misc:cc(?MODULE),
    %% read module source
    {ok, Binary} = file:read_file(beam:source(?MODULE)),
    FileData = binary_to_list(Binary),
    %% split execute command function part
    NewFileData = string:sub_string(FileData, string:rstr(FileData, "execute_command")),
    %% extract description and command define
    {match, MatchList} = re:run(NewFileData, "(?m)(?s)(?<=@doc).*?(?=\\-\\>)", [global, {capture, all, list}]),
    %% remove string quote " and list quote []
    List = [[[C || C <- string:strip(X), C =/= $" andalso C =/= $[ andalso C =/= $]] || X <- string:tokens(Command, "\n")] || [Command] <- MatchList],
    %% remove empty and replace the ',' to '_'
    NewList = [{Description, lists:flatten(string:replace(string:replace(Command, ",", "_", all), " ", "", all))} || [Description, Command] <- List],
    user_sender:send(SenderPid, 60001, NewList).

%% @doc cheat
-spec cheat(User :: #user{}, Command :: string()) -> ok() | error().
cheat(User, Command) ->
    case execute_command(User, Command, ?CHEAT) of
        {ok, NewUser = #user{}} ->
            {ok, ok, NewUser};
        Error ->
            Error
    end.

execute_command(_User, _Command, 0) ->
    ok;
execute_command(User = #user{role_id = RoleId, role_name = RoleName}, Command, _) ->
    case string:tokens(lists:flatten(string:replace(Command, " ", "", all)), "_") of
        %% @doc 登出
        ["logout"] ->
            gen_server:cast(self(), {stop, ok});
        %% @doc 充值, 充值Id
        ["recharge", RechargeId] ->
            #recharge_data{now_price = NowPrice} = recharge_data:get(type:to_integer(RechargeId)),
            Recharge = #recharge{
                recharge_id = type:to_integer(RechargeId),
                order_id = type:to_binary(time:millisecond()),
                channel = type:to_binary(?MODULE),
                role_id = RoleId,
                role_name = RoleName,
                money = NowPrice,
                time = time:now()
            },
            RechargeNo = recharge_sql:insert(Recharge),
            recharge:recharge(User, RechargeNo);
        %% @doc 等级
        ["level", Level] ->
            case role_data:exp(type:to_integer(Level)) - User#user.asset#asset.exp of
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
        %% @doc 物品Id, 数量
        ["item", ItemId, Number] ->
            item:add(User, [{type:to_integer(ItemId), type:to_integer(Number)}], ?MODULE);
        _ ->
            {error, no_such_command}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
