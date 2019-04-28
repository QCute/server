%%%-------------------------------------------------------------------
%%% @doc
%%% serialize define
%%% @end
%%%-------------------------------------------------------------------

-record(protocol, {name, comment, include, file, io}).
-record(io,       {name, comment, read, write}).
-record(param,    {name, comment, desc, extra}).
-record(list,     {name, comment, desc}).
-record(ets,      {name, comment, desc}).
-record(str,      {name, comment}).
-record(bin,      {name, comment}).
-record(u128,     {name, comment}).
-record(u64,      {name, comment}).
-record(u32,      {name, comment}).
-record(u16,      {name, comment}).
-record(u8,       {name, comment}).
-record(zero,     {}).