%%%-------------------------------------------------------------------
%%% @doc
%%% module mail handle
%%% @end
%%%-------------------------------------------------------------------
-module(mail_handle).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("mail.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc all
handle(?CMD_MAIL, #user{mail = Mails}, []) ->
    {reply, Mails};

%% @doc read
handle(?CMD_MAIL_READ, User, [MailId]) ->
    mail:read(User, MailId);

%% @doc receive attachment
handle(?CMD_MAIL_RECEIVE_ATTACHMENT, User, [MailId]) ->
    case mail:receive_attachment(User, MailId) of
        {ok, NewUser} ->
            {reply, [1], NewUser};
        {error, Code} ->
            {reply, [Code]};
        _ ->
            skip
    end;

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


