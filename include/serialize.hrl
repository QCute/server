%%%-------------------------------------------------------------------
%%% @doc
%%% serialize define
%%% @end
%%%-------------------------------------------------------------------

-record(protocol, {name, comment, include, file, io}).
-record(io,       {name, comment, read, write}).
-record(list,     {name, comment, explain}).
-record(ets,      {name, comment, explain}).
-record(str,      {name, comment}). %% string ""
-record(bst,      {name, comment}). %% bit string <<"">>
-record(fst,      {name, comment}). %% fix length binary <<"">>
-record(u128,     {name, comment}).
-record(u64,      {name, comment}).
-record(u32,      {name, comment}).
-record(u16,      {name, comment}).
-record(u8,       {name, comment}).
-record(zero,     {}).

