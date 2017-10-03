%%%-------------------------------------------------------------------
%%% @author raol
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2017 23:35
%%%-------------------------------------------------------------------
-module(parser_tests).
-author("raol").
-include_lib("eunit/include/eunit.hrl").
-include("telebot_api.hrl").


update_record_parse_test() ->
  Data = <<"{\"update_id\": 1000}">>,
  Update = telebot_parser:parse(update, Data),
  ?assertEqual(Update#update.update_id, 1000).

update_record_nested_message_test() ->
  Data = <<"{\"update_id\": 1000, \"message\" : { \"message_id\": 100 }}">>,
  Update = telebot_parser:parse(update, Data),
  Message = Update#update.message,
  ?assertEqual(Message#message.message_id, 100).

poll_record_message_list_test() ->
  Data = <<"{ \"ok\": true, \"result\": [{\"update_id\" : 100}]}">>,
  Poll = telebot_parser:parse(poll, Data),
  Msgs = Poll#poll.result,
  ?assertEqual(length(Msgs), 1).

poll_record_message_list_empty_test() ->
  Data = <<"{ \"ok\": true }">>,
  Poll = telebot_parser:parse(poll, Data),
  Msgs = Poll#poll.result,
  ?assertEqual(length(Msgs), 0).