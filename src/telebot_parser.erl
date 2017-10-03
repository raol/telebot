-module(telebot_parser).

-compile({parse_transform, exprecs}).
-include("telebot_api.hrl").
-export_records([update, message, chat, poll]).

%% API
-export([to_json/1, json_to_props/1, props_to_json/1, parse/2]).

%%====================================================================
%% API functions
%%====================================================================

json_to_props(Json) ->
  jsx:decode(Json, [{labels, atom}]).

props_to_json(List) when is_list(List) ->
  jsx:encode(List).

parse(Record, Payload) when is_binary(Payload) ->
  Props = jsx:decode(Payload, [{labels, atom}]),
  parse_props(Record, Props);

parse(Record, Payload) when is_list(Payload) ->
  parse_props(Record, Payload).

to_json(Rec) ->
  Props = to_props(Rec),
  props_to_json(Props).


%%====================================================================
%% Internal functions
%%====================================================================

to_props(Rec) when is_tuple(Rec) ->
  [{Key, to_props(Val)} || {Key, Val} <- to_list(Rec), Val =/= undefined];

to_props(V) -> V.

parse_props(_Record, undefined) -> undefined;

parse_props(Record, Props) when(is_list(Props)) ->
  handle_nested('#fromlist-'(Props, '#new-'(Record))).

handle_nested(R = #update{message = Msg}) ->
  R#update { message = parse_props(message, Msg)};

handle_nested(R = #message{chat = Chat}) ->
  R#message{ chat = parse_props(chat, Chat)};

handle_nested(R = #poll{ result = undefined }) ->
  R#poll{ result = [] };

handle_nested(R = #poll{ result = Res }) when is_list(Res) ->
  Msgs = lists:map(fun(M) -> parse_props(update, M) end, Res),
  R#poll{ result = Msgs };

handle_nested(Other) -> Other.

to_list(R) ->
  [Type|Fields] = tuple_to_list(R),
  lists:zip('#info-'(Type), Fields).