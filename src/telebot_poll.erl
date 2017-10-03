-module(telebot_poll).
-behavior(gen_server).

-include("constants.hrl").
-include("telebot_api.hrl").

%% Records

-record(state, {
  last_processed = 0,
  poll_interval,
  state=initializing,
  auth_token}).

%% API
-export([start_link/0, start/1, stop/0, new_offset/2]).

%% Gen Server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop() ->
  case whereis(?POLLER) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, stop),
      ok;
    undefined -> {error, poll_not_started}
  end.

start(SPid) ->
  case whereis(?POLLER) of
    Pid when is_pid(Pid) -> {error, already_started};
    undefined ->
      supervisor:start_child(SPid, {
        ?MODULE,
        {?MODULE, start_link, []},
        permanent,
        5000,
        worker,
        [?MODULE]
      })
  end.

%%====================================================================
%% Gen server callbacks
%%====================================================================

init([]) ->
  {ok, Auth} = application:get_env(auth_token),
  {ok, Interval} = application:get_env(poll_interval),
  % TODO: Read-store state
  {ok, #state{poll_interval = Interval, auth_token = Auth}, Interval}.

handle_call(_Msg, _From, S = #state{poll_interval = I}) ->
  {noreply, S, I}.

handle_cast(stop, State) ->
  % TODO: Save state on stop
  {stop, normal, State};

handle_cast(_Msg, S = #state{}) ->
  {noreply, S, S#state.poll_interval}.

handle_info(timeout, S = #state{state = initializing}) ->
  register(?POLLER, self()),
  {noreply, S#state{state = working}, S#state.poll_interval};

handle_info(timeout, S = #state{state = working, last_processed = Offset}) ->
  {ok, Updates} = telebot:get_updates(Offset),
  NewState = S#state{ last_processed = new_offset(Offset, Updates)},
  telebot_handler:handle_poll(Updates),
  % TODO: Get Last marker
  {noreply, NewState, S#state.poll_interval}.

terminate(_Reason, _State) ->
  error_logger:error_info("Received terminate in message poller~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%====================================================================
%% Private methods
%%====================================================================

new_offset(Current, []) -> Current;

new_offset(_Current, Updates) ->
  lists:max(lists:map(fun(U) -> U#update.update_id end, Updates)) + 1.