%%%------------------------------------------------------------------
%%% @doc
%%% module title
%%% @end
%%%------------------------------------------------------------------
-module(title).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([expire/1]).
-export([add/3, delete/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("title.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
	Title = title_sql:select(RoleId),
	User#user{title = Title}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{title = Title}) ->
	NewTitle = title_sql:insert_update(Title),
	User#user{title = NewTitle}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{title = Title}) ->
	{ok, Title}.

%% @doc expire
-spec expire(User :: #user{}) -> NewUser :: #user{}.
expire(User = #user{title = TitleList}) ->
	Now = time:ts(),
	{NewUser, NewList, Delete} = expire_loop(TitleList, User, Now, [], []),
	_ = Delete =/= [] andalso user_sender:send(User, ?PROTOCOL_TITLE_DELETE, Delete) == ok,
	NewUser#user{title = NewList}.

expire_loop([], User, _, List, Delete) ->
	{User, List, Delete};
expire_loop([Buff = #title{expire_time = 0} | T], User, Now, List, Delete) ->
	expire_loop(T, User, Now, [Buff | List], Delete);
expire_loop([Buff = #title{role_id = RoleId, title_id = TitleId, expire_time = ExpireTime} | T], User, Now, List, Delete) ->
	case Now < ExpireTime of
		true ->
			title_sql:delete(RoleId, TitleId),
			expire_loop(T, User, Now, List, [Buff | Delete]);
		false ->
			expire_loop(T, User, Now, [Buff | List], Delete)
	end.

%% @doc add
-spec add(User :: #user{}, TitleId :: non_neg_integer(), From :: term()) -> ok() | error().
add(User, TitleId, From) ->
	case title_data:get(TitleId) of
		TitleData = #title_data{} ->
			check_duplicate(User, TitleData, From);
		_ ->
			{error, configure_not_found}
	end.

check_duplicate(User = #user{title = TitleList}, TitleData = #title_data{title_id = TitleId}, From) ->
	case lists:keymember(TitleId, #title.title_id, TitleList) of
		false ->
			check_unique(User, TitleData, From);
		true ->
			{error, duplicate_title}
	end.

check_unique(User = #user{role_id = RoleId}, TitleData = #title_data{title_id = TitleId, type = Type, unique = false, time = Time}, From) ->
	NewTitle = #title{role_id = RoleId, title_id = TitleId, type = Type, expire_time = time:set_expire(Time)},
	add_new(User, NewTitle, TitleData, From);
check_unique(User = #user{role_id = RoleId}, TitleData = #title_data{title_id = TitleId, unique = true, time = Time}, From) ->
	case title_sql:select_id(TitleId) of
		[[OtherRoleId | _] = RawData] ->
			%% update database
			title_sql:update_role_id(RoleId, OtherRoleId, TitleId),
			%% notify delete
			user_server:apply_cast(OtherRoleId, ?MODULE, delete, [TitleId]),
			%% add to self
			Title = list_to_tuple([?MODULE | RawData]),
			NewTitle = Title#title{role_id = RoleId, expire_time = time:set_expire(Time)},
			add_new(User, NewTitle, TitleData, From);
		[] ->
			check_multi(User, TitleData, From)
	end.

check_multi(User, TitleData = #title_data{title_id = TitleId, type = Type, multi = true, time = Time}, From) ->
	NewTitle = #title{title_id = TitleId, type = Type, expire_time = time:set_expire(Time)},
	add_new(User, NewTitle, TitleData, From);
check_multi(User = #user{title = TitleList}, TitleData = #title_data{title_id = TitleId, type = Type, multi = false, time = Time}, From) ->
	case lists:keyfind(Type, #title.type, TitleList) of
		Title = #title{expire_time = ExpireTime} ->
			NewTitle = Title#title{title_id = TitleId, expire_time = time:set_expire(ExpireTime, Time, time:ts())};
		_ ->
			NewTitle = #title{title_id = TitleId, type = Type, expire_time = time:set_expire(Time)}
	end,
	add_new(User, NewTitle, TitleData, From).

add_new(User = #user{role_id = RoleId, title = TitleList}, Title = #title{title_id = TitleId}, #title_data{attribute = Attribute}, From) ->
	NewTitleList = lists:keystore(TitleId, #title.title_id, TitleList, Title),
	NewUser = attribute:calculate(User, ?MODULE, Attribute),
	log:title_log(RoleId, TitleId, From, time:ts()),
	{ok, NewUser#user{title = NewTitleList}}.

%% @doc delete
-spec delete(User :: #user{}, TitleId :: non_neg_integer()) -> ok() | error().
delete(User = #user{title = TitleList}, TitleId) ->
	case lists:keytake(TitleId, #title.title_id, TitleList) of
		{value, Title, NewTitleList} ->
			user_sender:send(User, ?PROTOCOL_TITLE_DELETE, [Title]),
			{ok, User#user{title = NewTitleList}};
		_ ->
			{error, no_such_title}
	end.
%%%==================================================================
%%% Internal functions
%%%==================================================================
