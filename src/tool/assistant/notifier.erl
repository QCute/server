%%%-------------------------------------------------------------------
%%% @doc
%%% exception notifier
%%% @end
%%%-------------------------------------------------------------------
-module(notifier).
%% API
-export([start/0, start_link/0, notify/3, clean_notify/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc start link
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc notify
-spec notify(Module :: atom(), Line :: non_neg_integer(), Reason :: term()) -> ok.
notify(Module, Line, Reason) ->
    gen_server:cast(?MODULE, {notify, Module, Line, Reason}).

%% @doc clean alert notifier
-spec clean_notify() -> ok.
clean_notify() ->
    gen_server:call(?MODULE, clean_notify).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init(_) ->
    inets:start(),
    ssl:start(),
    erlang:process_flag(trap_exit, true),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: list()) -> {reply, Reply :: term(), NewState :: list()}.
handle_call(clean_notify, _From, _State) ->
    {reply, ok, []};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_cast({notify, Module, Line, Reason}, State) ->
    try
        case lists:member({Module, Line}, State) of
            true ->
                {noreply, State};
            false ->
                %% read notify key file config in real time
                {ok, [[Home | _] | _]} = init:get_argument(home),
                File = lists:concat([Home, "/.notify/config"]),
                filelib:ensure_dir(File),
                not filelib:is_file(File) andalso file:write_file(File, <<>>),
                {ok, Data} = file:read_file(File),
                %% notify
                Title = escape_uri(lists:concat(["Server (Id: ", config:server_id(), ") Catch Exception!"])),
                Content = escape_uri(lists:flatten(string:replace(lists:flatten(Reason), "\n", "\n\n", all))),
                %% go to https://xizhi.qqoq.net/ get the sec key
                F = fun(Key) -> httpc:request(lists:concat(["https://xizhi.qqoq.net/", Key, ".send?title=", Title, "&content=", Content])) end,
                lists:foreach(F, string:tokens(binary_to_list(binary:replace(Data, <<"\r">>, <<>>, [global])), "\n")),
                {noreply, [{Module, Line} | State]}
        end
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        error_logger:error_msg(journal:format_stacktrace(Class, Reason, ?GET_STACKTRACE(Stacktrace)))
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: list()) -> {noreply, NewState :: list()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: list()) -> {ok, NewState :: list()}.
terminate(_, State) ->
    %% receiver closed
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: list(), Extra :: term()) -> {ok, NewState :: list()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% escape uri
escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].

escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
    N + $a - 10.
