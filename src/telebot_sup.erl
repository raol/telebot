-module(telebot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, {{one_for_one, 10, 1}, [{
    telebot_clientsup,
    {telebot_clientsup, start_link, []},
    permanent,
    5000,
    supervisor,
    [telebot_clientsup]
  }]}}.

%%====================================================================
%% Internal functions
%%====================================================================
