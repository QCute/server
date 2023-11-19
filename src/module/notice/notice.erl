%%%-------------------------------------------------------------------
%%% @doc
%%% notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([on_load/1, on_save/1]).
-export([query/1]).
-export([coming/2]).
-export([broadcast/4]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Notice = notice_role_sql:select(RoleId),
    List = notice_server:list(),
    collect_notice(List, User, Notice, []).

collect_notice([], User = #user{mail = MailList}, Notice, Mail) ->
    User#user{notice = Notice, mail = listing:merge(Mail, MailList)};
collect_notice([#notice{notice_id = NoticeId, type = Type, receive_time = ReceiveTime, expire_time = ExpireTime, title = Title, content = Content, attachment = Attachment} | T], User = #user{role_id = RoleId}, Notice, Mail) ->
    RoleNotice = #notice_role{notice_id = NoticeId, role_id = RoleId, receive_time = ReceiveTime, expire_time = ExpireTime, title = Title, content = Content, flag = 1},
    case lists:keymember(NoticeId, #notice_role.notice_id, Notice) of
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

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{notice = Notice}) ->
    NewNotice = mail_sql:save(Notice),
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
broadcast(Scope, Type, Text, _) when is_atom(Text) ->
    Content = text_data:text(Text),
    {ok, Binary} = user_router:encode(?PROTOCOL_NOTICE_BROADCAST, {Scope, Type, <<>>, Content}),
    user_manager:broadcast(Binary);
broadcast(Scope, Type, Title, Content) when is_binary(Title) ->
    {ok, Binary} = user_router:encode(?PROTOCOL_NOTICE_BROADCAST, {Scope, Type, Title, Content}),
    user_manager:broadcast(Binary).

%%%===================================================================
%%% Internal functions
%%%===================================================================
