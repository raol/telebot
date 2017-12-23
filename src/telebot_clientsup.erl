-module(telebot_clientsup).
-behaviour(supervisor).

%% API
-export([start_link/0, add_chat/1]).

%% Supervisor callback
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, Handler} = application:get_env(telebot, bot_handler),
  {ok, {{simple_one_for_one, 10, 1}, [{
    telebot_bot,
    {Handler, start_link, []},
    permanent,
    500,
    worker,
    [Handler]
  }]}}.

add_chat(ChatId) ->
  supervisor:start_child(?MODULE, [ChatId]).

