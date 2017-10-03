-module(telebot_http).

-define(URL, <<"https://api.telegram.org/bot">>).

%% API
-export([request/3]).

%%====================================================================
%% API
%%====================================================================

request(Method, ReplyType, ReqOpts) ->
  Url = make_url(Method),
  Options = [{with_body, true}],
  Body = telebot_parser:props_to_json(ReqOpts),
  Result = hackney:post(Url, [{<<"Content-Type">>, <<"application/json">>}], Body, Options),
  unwrap_result(Result, ReplyType).


%%====================================================================
%% Internal Functions
%%====================================================================

make_url(Method) ->
  {ok, Token} = application:get_env(telebot, auth_token),
  lists:flatten(io_lib:format("~s~s/~s", [?URL, Token, Method])).

unwrap_result({ok, 200, _, Body}, RecordType) ->
  case telebot_parser:json_to_props(Body) of
    [{ok, true}, {result, Result}] ->
      case RecordType of
        undefined -> ok;

        [Rt] -> {ok, [telebot_parser:parse(Rt, R) || R <- Result]};

        _ -> {ok, telebot_parser:parse(RecordType, Result)}
      end;
    [{ok, false}, {result, _}] ->
      % TODO: Return handle error
      {error, error}
  end;

unwrap_result({ok, _, _, _ClientRef}, _) ->
  {error}.