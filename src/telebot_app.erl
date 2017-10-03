-module(telebot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ok = configure_webhook(),
  ok = configure_processors(),
  {ok, SPid} = telebot_sup:start_link(),
  {ok, Poll} = application:get_env(telebot, start_poll),
  syn:init(),
  ok = case Poll of
    true ->
      telebot_poll:start(SPid),
      ok;
    _ -> ok
  end,
  {ok, SPid}.


%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

configure_webhook() ->
  {ok, Port} = application:get_env(telebot, port),
  Dispatch = cowboy_router:compile([
    {'_', [{"/", telebot_webhook, []}]}
  ]),
  cowboy:start_http(my_http_listener,
    100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]),
  ok.

configure_processors() ->
  {ok, Workers} = application:get_env(telebot, workers),
  wpool:start_pool(message_processors, [{workers, Workers}]),
  ok.