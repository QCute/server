%%%-------------------------------------------------------------------
%%% @doc
%%% notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([coming/2]).
-export([broadcast/4]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> #user{}.
load(User = #user{role_id = RoleId}) ->
    Notice = role_notice_sql:select_join_by_role_id(RoleId),
    List = notice_server:list(),
    collect_notice(List, User, Notice, []).

collect_notice([], User = #user{mail = MailList}, Notice, Mail) ->
    User#user{notice = Notice, mail = listing:merge(Mail, MailList)};
collect_notice([#notice{notice_id = NoticeId, type = Type, receive_time = ReceiveTime, expire_time = ExpireTime, title = Title, content = Content, attachment = Attachment} | T], User = #user{role_id = RoleId}, Notice, Mail) ->
    RoleNotice = #role_notice{notice_id = NoticeId, role_id = RoleId, receive_time = ReceiveTime, expire_time = ExpireTime, title = Title, content = Content, flag = 1},
    case lists:keymember(NoticeId, #role_notice.notice_id, Notice) of
        false when Type == ?NOTICE_TYPE_MAIL ->
            %% the mail type notice
            NewMails = mail:make(RoleId, Title, Content, ?NOTICE, Attachment, []),
            collect_notice(T, User, [RoleNotice | Notice], listing:merge(NewMails, Mail));
        false ->
            %% normal notice
            collect_notice(T, User, [RoleNotice | Notice], Mail);
        true ->
            collect_notice(T, User, Notice, Mail)
    end.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{notice = Notice}) ->
    NewNotice = mail_sql:insert_update(Notice),
    User#user{notice = NewNotice}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{notice = Notice}) ->
    {ok, Notice}.

%% @doc notice coming
-spec coming(User :: #user{}, Notice :: #notice{}) -> {ok, #user{}}.
coming(User = #user{notice = NoticeList}, Notice) ->
    NewUser = #user{notice = NewNoticeList} = collect_notice([Notice], User, NoticeList, []),
    user_sender:send(?PROTOCOL_NOTICE_QUERY, NewNoticeList),
    {ok, NewUser}.

%% @doc broadcast
-spec broadcast(Scope :: non_neg_integer(), Type :: non_neg_integer(), Text :: atom() | binary(), Content :: [term()]) -> ok.
broadcast(Scope, Type, Text, Args) when is_atom(Text) ->
    Content = parser:format(text_data:text(Text), Args),
    {ok, Binary} = user_router:write(?PROTOCOL_NOTICE_BROADCAST, [Scope, Type, <<>>, Content]),
    user_manager:broadcast(Binary);
broadcast(Scope, Type, Title, Content) when is_binary(Title) ->
    {ok, Binary} = user_router:write(?PROTOCOL_NOTICE_BROADCAST, [Scope, Type, Title, Content]),
    user_manager:broadcast(Binary).

%%%===================================================================
%%% Internal functions
%%%===================================================================
