-record(bot_state, {
  id :: integer(),
  status :: idle | working,
  state
}).