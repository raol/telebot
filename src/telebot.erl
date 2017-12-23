-module(telebot).

%% API
-export([get_updates/1, send_message/2, send_message/3, start_poll/0, set_bot_handler/1, set_auth_token/1]).

set_bot_handler(Handler) ->
  application:set_env(telebot, bot_handler, Handler).

set_auth_token(Token) ->
  application:set_env(telebot, auth_token, Token).

start_poll() ->
  SPid = whereis(telebot_sup),
  telebot_poll:start(SPid).

get_updates(Offset) ->
  telebot_http:request(<<"getUpdates">>, [update], [{offset, Offset}]).

send_message(ChatId, Text) ->
  telebot_http:request(
    <<"sendMessage">>,
    message,
    [{chat_id, ChatId}, {text, Text}]).

send_message(ChatId, Text, Buttons) ->
  telebot_http:request(
    <<"sendMessage">>,
    message,
    [{chat_id, ChatId}, {text, Text}, {reply_markup, [{keyboard, Buttons}]}]).