-record(chat, {
  id :: integer(),
  last_name :: binary(),
  first_name :: binary(),
  username :: binary()
}).

-record(message_entity, {
  offset :: integer(),
  legnth :: integer(),
  type :: binary()
}).

-record(message, {
  message_id :: integer(),
  chat :: #chat{},
  text :: binary(),
  entities :: [#message_entity{}]
}).

-record(update, {
  update_id :: integer(),
  message :: binary()
}).

-record(poll, {
  ok :: boolean(),
  result :: [#update{}]
}).