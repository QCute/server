%%%-------------------------------------------------------------------
%%% @doc
%%% module mail
%%% @end
%%%-------------------------------------------------------------------
-module(mail).
%% API
-export([load/1, save/1, expire/1]).
-export([query/1]).
-export([read/2, receive_attachment/2]).
-export([add/5, send/6, send/5, delete/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("item.hrl").
-include("mail.hrl").
%% Macros
-define(MAIL_MAX_ITEM,                                10).                  %% 单封邮件物品上限
-define(MAIL_VALID_DATE,                              ?DAY_SECONDS(15)).    %% 邮件有效时间
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Mail = mail_sql:select(RoleId),
    User#user{mail = Mail}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{mail = Mail}) ->
    NewMail = mail_sql:insert_update(Mail),
    User#user{mail = NewMail}.

%% @doc expire
-spec expire(User :: #user{}) -> NewUser :: #user{}.
expire(User = #user{mail = MailList}) ->
    Now = time:ts(),
    %% delete 15 day before, read or received attachment mail
    {Delete, Remain} = lists:partition(fun(#mail{is_read = IsRead, is_receive_attachment = IsReceiveAttachment, attachment = Attachment, expire_time = ExpireTime}) -> (Attachment == [] andalso IsRead == ?TRUE andalso ExpireTime =< Now - ?MAIL_VALID_DATE) orelse (IsReceiveAttachment == ?TRUE andalso ExpireTime =< Now - ?MAIL_VALID_DATE) end, MailList),
    mail_sql:delete_in_mail_id(listing:collect(#mail.mail_id, Delete)),
    User#user{mail = Remain}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{mail = Mail}) ->
    {ok, Mail}.

%% @doc read
-spec read(User :: #user{}, MailId :: non_neg_integer()) -> ok().
read(User = #user{mail = MailList}, MailId) ->
    case lists:keyfind(MailId, #mail.mail_id, MailList) of
        Mail = #mail{is_read = 0} ->
            NewMail = Mail#mail{is_read = 1},
            NewList = lists:keyreplace(MailId, #mail.mail_id, MailList, NewMail),
            mail_sql:update_read(time:ts(), ?TRUE, MailId),
            {ok, ok, User#user{mail = NewList}};
        #mail{} ->
            {error, already_read};
        _ ->
            {error, no_such_mail}
    end.

%% @doc receive attachment
-spec receive_attachment(User :: #user{}, MailId :: non_neg_integer()) -> ok() | error().
receive_attachment(User = #user{mail = Mail}, MailId) ->
    case lists:keyfind(MailId, #mail.mail_id, Mail) of
        #mail{attachment = Attachment} when Attachment =/= [] ->
            %% @todo receive item empty grid check strict(now)/permissive(if need)
            ItemList = item:classify(Attachment),
            {_, Items} = listing:key_find(?ITEM_TYPE_COMMON, 1, ItemList, {?ITEM_TYPE_COMMON, []}),
            {_, Bags} = listing:key_find(?ITEM_TYPE_BAG, 1, ItemList, {?ITEM_TYPE_BAG, []}),
            case (Items =/= [] andalso length(Items) < item:empty_grid(User, ?ITEM_TYPE_COMMON)) andalso (Bags =/= [] andalso length(Bags) < item:empty_grid(User, ?ITEM_TYPE_BAG)) of
                true ->
                    mail_sql:update_receive(time:ts(), ?TRUE, MailId),
                    {ok, NewUser} = item:add(User, Items, ?MODULE),
                    {ok, ok, NewUser};
                _ ->
                    {error, bag_full}
            end;
        #mail{} ->
            {error, no_attachment};
        _ ->
            {error, no_such_mail}
    end.

%% @doc add mail to self (sync call)
-spec add(User :: #user{}, Title :: binary() | atom(), Content :: binary() | atom(), From :: term(), Items :: list()) -> User :: #user{}.
add(User, Title, Content, From, Items) when is_atom(Title) ->
    add(User, text_data:get(Title), Content, From, Items);
add(User, Title, Content, From, Items) when is_atom(Content) ->
    add(User, Title, text_data:get(Content), From, Items);
add(User = #user{role_id = RoleId, role_name = RoleName, mail = MailList}, Title, Content, From, Items) ->
    NewMailList = make(RoleId, RoleName, Title, Content, From, Items, []),
    user_sender:send(User, ?PROTOCOL_MAIL, NewMailList),
    User#user{mail = listing:merge(NewMailList, MailList)}.

%% @doc send mail to role (async call)
-spec send(RoleId :: non_neg_integer(), RoleName :: binary(), Title :: binary() | atom(), Content :: binary() | atom(), From :: term(), Items :: list()) -> ok.
send(RoleId, RoleName, Title, Content, From, Items) when is_atom(Title) ->
    send(RoleId, RoleName, text_data:get(Title), Content, From, Items);
send(RoleId, RoleName, Title, Content, From, Items) when is_atom(Content) ->
    send(RoleId, RoleName, Title, text_data:get(Content), From, Items);
send(RoleId, RoleName, Title, Content, From, Items) ->
    NewMailList = make(RoleId, RoleName, Title, Content, From, Items, []),
    %% apply cast (async)
    user_server:apply_cast(RoleId, fun coming/2, [NewMailList]).

%% @doc send mail to role list(async call)
-spec send(RoleList :: [{RoleId :: non_neg_integer(), RoleName :: binary()}], Title :: binary() | atom(), Content :: binary() | atom(), From :: term(), Items :: list()) -> ok.
send(List, Title, Content, From, Items) ->
    [send(RoleId, RoleName, Title, Content, From, Items) || {RoleId, RoleName} <- List],
    ok.

%% @doc coming (async sending callback)
-spec coming(User :: #user{}, Mails :: list()) -> ok().
coming(User = #user{mail = MailList}, Mails) ->
    user_sender:send(User, ?PROTOCOL_MAIL, Mails),
    {ok, User#user{mail = listing:merge(Mails, MailList)}}.

%% @doc delete
-spec delete(User :: #user{}, MailId :: non_neg_integer()) -> ok().
delete(User = #user{mail = MailList}, MailId) ->
    NewMailList = lists:keydelete(MailId, #mail.mail_id, MailList),
    mail_sql:delete(MailId),
    {ok, ok, User#user{mail = NewMailList}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% split attachment
make(Receiver, Name, Title, Content, From, Items, Mails) when length(Items) =< ?MAIL_MAX_ITEM ->
    Mail = mail(Receiver, Name, Title, Content, From, Items),
    [Mail| Mails];
make(Receiver, Name, Title, Content, From, [A, B, C, D, E, F, G, H, I, J | Items], Mails) ->
    Mail = mail(Receiver, Name, Title, Content, From, [A, B, C, D, E, F, G, H, I, J]),
    make(Receiver, Name, Title, Content, From, Items, [Mail | Mails]).

%% make a new mail
mail(Receiver, Name, Title, Content, From, Items) ->
    Now = time:ts(),
    Mail = #mail{
        receiver_id = Receiver,
        receiver_nick = Name,
        attachment = Items,
        title = Title,
        content = Content,
        receive_time = Now,
        expire_time = Now + ?MAIL_VALID_DATE,
        from = type:to_binary(From),
        flag = 1
    },
    %% MailId = mail_sql:insert(Mail),
    MailId = increment_server:next(?MODULE),
    Mail#mail{mail_id = MailId}.
