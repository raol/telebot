-module(telebot_bot).
-behaviour(gen_server).

-include("telebot_api.hrl").
-define(LIVETIMEOUT, 10 * 60 * 1000).
-define(MINUTES(X), X * 1000).

%% API
-export([start_link/1]).

-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
  chat_id :: integer(),
  status = idle,
  interval_end :: integer()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Id) ->
  gen_server:start_link({via, syn, integer_to_binary(Id)}, ?MODULE, [Id], []).

%%====================================================================
%% Gen Server callbacks
%%====================================================================

init([Id]) ->
  {ok, #state{chat_id = Id}}.

%%====================================================================
%% Private functions
%%====================================================================

handle_cast(#message{ text = <<"/start">>}, S = #state{ status = idle }) ->
  Buttons = [[[{<<"text">>, <<"/short">>}], [{<<"text">>, <<"/long">>}]], [[{<<"text">>, <<"/cancel">>}]]],
  {ok, _} = telebot:send_message(S#state.chat_id, <<"Hello to the pomodoro bot">>, Buttons),
  {noreply, S, ?LIVETIMEOUT};

handle_cast(#message{ text = <<"/long">>}, S = #state{ status = idle }) ->
  ChatId = S#state.chat_id,
  {ok, _} = telebot:send_message(ChatId, <<"Started 25 min interval">>),
  End = os:system_time(millisecond) + ?MINUTES(25),
  {noreply, S#state{ status = interval, interval_end = End}, ?MINUTES(25)};

handle_cast(#message { text = <<"/short">>}, S = #state{ status = idle }) ->
  ChatId = S#state.chat_id,
  {ok, _} = telebot:send_message(ChatId, <<"Started 10 min interval">>),
  End = os:system_time(millisecond) + ?MINUTES(10),
  {noreply, S#state{ status = interval, interval_end = End}, ?MINUTES(10)};

handle_cast(#message { text = <<"/cancel">>}, S = #state{ status = interval }) ->
  ChatId = S#state.chat_id,
  {ok, _} = telebot:send_message(ChatId, <<"Cancelled interval">>),
  {noreply, S#state{ status = idle }, ?LIVETIMEOUT};

handle_cast(_, S = #state{ status = interval, interval_end = End}) ->
  Timeout = End - os:system_time(millisecond),
  {noreply, S, Timeout};

handle_cast(_, S = #state { status = idle }) ->
  {noreply, S, ?LIVETIMEOUT}.

handle_info(timeout, S = #state{ status = idle }) ->
  {stop, normal, S};

handle_info(timeout, S = #state{ status = interval, chat_id = ChatId}) ->
  telebot:send_message(ChatId, <<"Your interval is over">>),
  {noreply, S#state{ status = idle }, ?LIVETIMEOUT}.

terminate(_Reason, _State) ->
  ok.