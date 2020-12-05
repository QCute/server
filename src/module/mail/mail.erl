%%%-------------------------------------------------------------------
%%% @doc
%%% mail
%%% @end
%%%-------------------------------------------------------------------
-module(mail).
%% API
-export([load/1, save/1, expire/1]).
-export([query/1]).
-export([read/2, receive_attachment/2]).
-export([add/5, send/5, delete/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("item.hrl").
-include("mail.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Mail = mail_sql:select_by_role_id(RoleId),
    User#user{mail = Mail}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{mail = Mail}) ->
    NewMail = mail_sql:insert_update(Mail),
    User#user{mail = NewMail}.

%% @doc expire
-spec expire(User :: #user{}) -> NewUser :: #user{}.
expire(User = #user{mail = MailList}) ->
    Now = time:now(),
    %% delete 7 day before, read(no attachment) or received attachment mail
    {Delete, Remain} = lists:partition(fun(#mail{read_time = ReadTime, receive_attachment_time = ReceiveAttachmentTime, attachment = Attachment}) -> (Attachment == [] andalso ReadTime =/= 0 andalso ReadTime + parameter_data:get(mail_expire_time) =< Now) orelse (ReceiveAttachmentTime =/= 0 andalso ReceiveAttachmentTime + parameter_data:get(mail_expire_time) =< Now) end, MailList),
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
            mail_sql:update_read(time:now(), ?TRUE, MailId),
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
                    mail_sql:update_receive(time:now(), ?TRUE, MailId),
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
    add(User, tool:text(Title), Content, From, Items);
add(User, Title, Content, From, Items) when is_atom(Content) ->
    add(User, Title, tool:text(Content), From, Items);
add(User = #user{role_id = RoleId, mail = MailList}, Title, Content, From, Items) ->
    NewMailList = make(RoleId, Title, Content, From, Items, []),
    user_sender:send(User, ?PROTOCOL_MAIL_QUERY, NewMailList),
    User#user{mail = listing:merge(NewMailList, MailList)}.

%% @doc send mail to role (async call)
-spec send(RoleId :: non_neg_integer() | [RoleId :: non_neg_integer()], Title :: binary() | atom(), Content :: binary() | atom(), From :: term(), Items :: list()) -> ok.
send(List = [_ | _], Title, Content, From, Items) ->
    [send(RoleId, Title, Content, From, Items) || RoleId <- List],
    ok;
send(RoleId, Title, Content, From, Items) when is_atom(Title) ->
    send(RoleId, tool:text(Title), Content, From, Items);
send(RoleId, Title, Content, From, Items) when is_atom(Content) ->
    send(RoleId, Title, tool:text(Content), From, Items);
send(RoleId, Title, Content, From, Items) ->
    %% make mail
    MailList = make(RoleId, Title, Content, From, Items, []),
    %% save to database
    NewMailList = mail_sql:insert_update(MailList),
    %% apply cast (async)
    user_server:apply_cast(RoleId, fun coming/2, [NewMailList]).

%% coming (async sending callback)
coming(User = #user{mail = MailList}, NewMailList) ->
    user_sender:send(User, ?PROTOCOL_MAIL_QUERY, NewMailList),
    {ok, User#user{mail = listing:merge(NewMailList, MailList)}}.

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
make(Receiver, Title, Content, From, Items, Mails) ->
    case parameter_data:get(mail_max_item) < length(Items) of
        true ->
            {SplitItems, RemainItems} = lists:split(parameter_data:get(mail_max_item), Items),
            Mail = #mail{mail_id = increment_server:next(?MODULE), receiver_id = Receiver, attachment = SplitItems, title = Title, content = Content, receive_time = time:now(), from = From, flag = 1},
            make(Receiver, Title, Content, From, RemainItems, [Mail | Mails]);
        false ->
            Mail = #mail{mail_id = increment_server:next(?MODULE), receiver_id = Receiver, attachment = Items, title = Title, content = Content, receive_time = time:now(), from = From, flag = 1},
            [Mail| Mails]
    end.
