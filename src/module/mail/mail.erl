%%%-------------------------------------------------------------------
%%% @doc
%%% mail
%%% @end
%%%-------------------------------------------------------------------
-module(mail).
%% API
-export([on_load/1, on_save/1, on_expire/1]).
-export([query/1]).
-export([read/2, receive_attachment/2]).
-export([add/5, send/5, delete/2]).
-export([make/6]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("item.hrl").
-include("mail.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    Mail = mail_sql:select(RoleId),
    User#user{mail = Mail}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{mail = Mail}) ->
    NewMail = mail_sql:save(Mail),
    User#user{mail = NewMail}.

%% @doc on expire
-spec on_expire(User :: #user{}) -> NewUser :: #user{}.
on_expire(User = #user{mail = MailList}) ->
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
        Mail = #mail{read_time = 0} ->
            Now = time:now(),
            NewMail = Mail#mail{read_time = Now},
            NewList = lists:keyreplace(MailId, #mail.mail_id, MailList, NewMail),
            mail_sql:update_read(NewMail),
            {ok, ok, User#user{mail = NewList}};
        #mail{} ->
            {error, mail_already_read};
        _ ->
            {error, mail_not_found}
    end.

%% @doc receive attachment
-spec receive_attachment(User :: #user{}, MailId :: non_neg_integer()) -> ok() | error().
receive_attachment(User = #user{mail = MailList}, MailId) ->
    case lists:keyfind(MailId, #mail.mail_id, MailList) of
        Mail = #mail{receive_attachment_time = 0, attachment = Attachment = [_ | _]} ->
            %% @todo receive item empty grid check strict(now)/permissive(if need)
            ItemList = item:classify(Attachment),
            {_, Items} = listing:key_find(?ITEM_TYPE_COMMON, 1, ItemList, {?ITEM_TYPE_COMMON, []}),
            {_, Bags} = listing:key_find(?ITEM_TYPE_BAG, 1, ItemList, {?ITEM_TYPE_BAG, []}),
            case (length(Items) < item:empty_grid(User, ?ITEM_TYPE_COMMON)) andalso (length(Bags) < item:empty_grid(User, ?ITEM_TYPE_BAG)) of
                true ->
                    Now = time:now(),
                    NewMail = Mail#mail{receive_attachment_time = Now},
                    NewList = lists:keyreplace(MailId, #mail.mail_id, MailList, NewMail),
                    mail_sql:update_attachment(NewMail),
                    {ok, NewUser} = item:add(User#user{mail = NewList}, Items, ?MODULE),
                    {ok, ok, NewUser};
                _ ->
                    {error, item_bag_full}
            end;
        #mail{attachment = []} ->
            {error, mail_attachment_empty};
        #mail{} ->
            {error, mail_attachment_already_received};
        _ ->
            {error, mail_not_found}
    end.

%% @doc add mail to self (sync call)
-spec add(User :: #user{}, Title :: binary() | atom(), Content :: binary() | atom(), From :: term(), Items :: list()) -> User :: #user{}.
add(User, Title, Content, From, Items) when is_atom(Title) ->
    add(User, text_data:text(Title), Content, From, Items);
add(User, Title, Content, From, Items) when is_atom(Content) ->
    add(User, Title, text_data:text(Content), From, Items);
add(User = #user{role_id = RoleId, mail = MailList}, Title, Content, From, Items) ->
    NewMailList = make(RoleId, Title, Content, From, Items, []),
    user_sender:send(User, ?PROTOCOL_MAIL_QUERY, NewMailList),
    User#user{mail = listing:merge(NewMailList, MailList)}.

%% @doc send mail to role (async call)
-spec send(RoleId :: non_neg_integer() | [RoleId :: non_neg_integer()], Title :: binary() | atom(), Content :: binary() | atom(), From :: term(), Items :: list()) -> ok.
send(List, Title, Content, From, Items) when is_list(List) ->
    NewMailList = lists:foldl(fun(RoleId, Acc) ->
        MailList = make(RoleId, Title, Content, From, Items, []),
        user_server:apply_cast(RoleId, fun coming/2, [MailList]),
        listing:merge(MailList, Acc)
    end, [], List),
    mail_sql:save(NewMailList),
    ok;
send(RoleId, Title, Content, From, Items) when is_atom(Title) ->
    send(RoleId, text_data:text(Title), Content, From, Items);
send(RoleId, Title, Content, From, Items) when is_atom(Content) ->
    send(RoleId, Title, text_data:text(Content), From, Items);
send(RoleId, Title, Content, From, Items) ->
    %% make mail
    MailList = make(RoleId, Title, Content, From, Items, []),
    %% save to database
    NewMailList = mail_sql:save(MailList),
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

%% @doc make split attachment mail
-spec make(RoleId :: non_neg_integer(), Title :: binary(), Content :: binary(), From :: term(), Items :: list(), Mails :: [#mail{}]) -> [#mail{}].
make(RoleId, Title, Content, From, Items, Mails) ->
    Now = time:now(),
    MaxItem = parameter_data:get(mail_max_item),
    case MaxItem < length(Items) of
        true ->
            {SplitItems, RemainItems} = lists:split(MaxItem, Items),
            Mail = #mail{mail_id = increment_server:next(?MODULE), role_id = RoleId, receive_time = Now, expire_time = Now + parameter_data:get(mail_expire_time), title = Title, content = Content, attachment = SplitItems, from = From, flag = 1},
            make(RoleId, Title, Content, From, RemainItems, [Mail | Mails]);
        false ->
            Mail = #mail{mail_id = increment_server:next(?MODULE), role_id = RoleId, receive_time = Now, expire_time = Now + parameter_data:get(mail_expire_time), title = Title, content = Content, attachment = Items, from = From, flag = 1},
            [Mail| Mails]
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
