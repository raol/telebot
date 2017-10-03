-module(telebot_handler).
-include("telebot_api.hrl").

%% API
-export([handle_poll/1, handle_hook/1]).


handle_hook(Data) ->
  Msg = telebot_parser:parse(update, Data),
  io:format("Got ~w", [Msg]),
  dispatch(Msg).

handle_poll([]) ->
  ok;

handle_poll([Msg = #update{} | Cont]) ->
  dispatch(Msg),
  handle_poll(Cont).

%%====================================================================
%% Private methods
%%====================================================================

dispatch(Msg = #update{}) ->
  Message = Msg#update.message,
  ChatId = Message#message.chat#chat.id,
  case syn:find_by_key(integer_to_binary(ChatId)) of
    undefined ->
      {ok, NPid} = telebot_clientsup:add_chat(ChatId),
      gen_server:cast(NPid, Message);
    Pid ->
      gen_server:cast(Pid, Message)
  end,
  ok.