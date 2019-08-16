%%%-------------------------------------------------------------------
%%% @doc
%%% serialize define
%%% @end
%%%-------------------------------------------------------------------

-record(protocol, {name = [], comment = [], includes = [], io = [], erl, json, lua}).
-record(io,       {name = [], comment = [], read = [], write = []}).
-record(tuple,    {name = [], comment = [], explain = []}).
-record(record,   {name = [], comment = [], explain = []}).
-record(list,     {name = [], comment = [], explain = []}).
-record(ets,      {name = [], comment = [], explain = []}).
-record(str,      {name = [], comment = [], explain = []}). %% string ""
-record(bst,      {name = [], comment = [], explain = []}). %% bit string <<"">>
-record(binary,   {name = [], comment = [], explain = 0}). %% fix length binary
-record(u128,     {name = [], comment = []}).
-record(u64,      {name = [], comment = []}).
-record(u32,      {name = [], comment = []}).
-record(u16,      {name = [], comment = []}).
-record(u8,       {name = [], comment = []}).
-record(i128,     {name = [], comment = []}).
-record(i64,      {name = [], comment = []}).
-record(i32,      {name = [], comment = []}).
-record(i16,      {name = [], comment = []}).
-record(i8,       {name = [], comment = []}).
-record(zero,     {}).
