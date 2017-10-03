-module(telebot_webhook).

%% API
-export([init/3, handle/2, terminate/3]).

%% Cowboy handlers

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, no_state) ->
  % TODO: Process Data variable
  {ok, Data, _} = cowboy_req:body(Req),
  {ok, Rep} = cowboy_req:reply(200, [], <<"">>, Req),
  wpool:cast(message_processors, {telebot_handler, handle_hook, [Data]}),
  {ok, Rep, no_state}.

terminate(_Reason, _Req, no_state) ->
  ok.
