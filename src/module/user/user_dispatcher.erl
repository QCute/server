%%%-------------------------------------------------------------------
%%% @doc
%%% user event dispatcher
%%% @end
%%%-------------------------------------------------------------------
-module(user_dispatcher).
%% API
-export([trigger/2]).
%% Includes
-include("event.hrl").
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc trigger static event
-spec trigger(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
%% trigger static event @here
%% auto generate, do not edit this code

trigger(User, Event = #event{name = charge}) ->
    CountUser = count:on_charge(User, Event),
    vip:on_charge(CountUser, Event);
trigger(User, #event{name = save}) ->
    AchievementUser = achievement:on_save(User),
    AssetUser = asset:on_save(AchievementUser),
    BubbleUser = bubble:on_save(AssetUser),
    BuffUser = buff:on_save(BubbleUser),
    ChargeUser = charge:on_save(BuffUser),
    ChatUser = chat:on_save(ChargeUser),
    CountUser = count:on_save(ChatUser),
    DailyUser = daily:on_save(CountUser),
    DeviceUser = device:on_save(DailyUser),
    DungeonUser = dungeon:on_save(DeviceUser),
    FashionUser = fashion:on_save(DungeonUser),
    FriendUser = friend:on_save(FashionUser),
    ItemUser = item:on_save(FriendUser),
    LocationUser = location:on_save(ItemUser),
    MailUser = mail:on_save(LocationUser),
    NoticeUser = notice:on_save(MailUser),
    PackageUser = package:on_save(NoticeUser),
    RoleUser = role:on_save(PackageUser),
    ShopUser = shop:on_save(RoleUser),
    SkillUser = skill:on_save(ShopUser),
    TaskUser = task:on_save(SkillUser),
    TitleUser = title:on_save(TaskUser),
    vip:on_save(TitleUser);
trigger(User, #event{name = load}) ->
    AchievementUser = achievement:on_load(User),
    AssetUser = asset:on_load(AchievementUser),
    BubbleUser = bubble:on_load(AssetUser),
    BuffUser = buff:on_load(BubbleUser),
    ChargeUser = charge:on_load(BuffUser),
    ChatUser = chat:on_load(ChargeUser),
    CountUser = count:on_load(ChatUser),
    DailyUser = daily:on_load(CountUser),
    DeviceUser = device:on_load(DailyUser),
    DungeonUser = dungeon:on_load(DeviceUser),
    FashionUser = fashion:on_load(DungeonUser),
    FriendUser = friend:on_load(FashionUser),
    ItemUser = item:on_load(FriendUser),
    LocationUser = location:on_load(ItemUser),
    MailUser = mail:on_load(LocationUser),
    NoticeUser = notice:on_load(MailUser),
    PackageUser = package:on_load(NoticeUser),
    RoleUser = role:on_load(PackageUser),
    ShopUser = shop:on_load(RoleUser),
    SignUser = sign:on_load(ShopUser),
    SkillUser = skill:on_load(SignUser),
    TaskUser = task:on_load(SkillUser),
    TitleUser = title:on_load(TaskUser),
    vip:on_load(TitleUser);
trigger(User, #event{name = expire}) ->
    BubbleUser = bubble:on_expire(User),
    BuffUser = buff:on_expire(BubbleUser),
    FashionUser = fashion:on_expire(BuffUser),
    ItemUser = item:on_expire(FashionUser),
    MailUser = mail:on_expire(ItemUser),
    title:on_expire(MailUser);
trigger(User, #event{name = reset}) ->
    ChargeUser = charge:on_reset(User),
    CountUser = count:on_reset(ChargeUser),
    DailyUser = daily:on_reset(CountUser),
    DungeonUser = dungeon:on_reset(DailyUser),
    ShopUser = shop:on_reset(DungeonUser),
    sign:on_reset(ShopUser);
trigger(User, #event{name = exp_add}) ->
    role:on_exp_add(User);
trigger(User, #event{name = disconnect}) ->
    role:on_disconnect(User);
trigger(User, #event{name = reconnect}) ->
    role:on_reconnect(User);
trigger(User, #event{name = logout}) ->
    role:on_logout(User);
trigger(User, #event{name = login}) ->
    role:on_login(User);
trigger(User, #event{name = create}) ->
    device:on_create(User);
trigger(User, Event = #event{name = shop_buy}) ->
    count:on_shop_buy(User, Event);
trigger(User, Event = #event{name = gold_cost}) ->
    count:on_gold_cost(User, Event);
trigger(User, _) ->
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
