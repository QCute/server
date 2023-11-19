-ifndef(SQL_HRL).
-define(SQL_HRL, 'SQL_HRL').
%%%-------------------------------------------------------------------
%%% @doc
%%% sql define
%%% @end
%%%-------------------------------------------------------------------
-compile({no_auto_import, [min/2, max/2]}).
-compile({nowarn_unused_record, [sql]}).
-compile({nowarn_unused_function, [raw/1]}).
-compile({nowarn_unused_function, [param/0]}).
-compile({nowarn_unused_function, [avg/1, bit_and/1, bit_or/1, bit_xor/1, count/1, max/1, min/1, std/1, std_dev/1, std_dev_pop/1, std_dev_sample/1, sum/1, var_pop/1, var_sample/1, variance/1]}).
-compile({nowarn_unused_function, [all/1]}).
-compile({nowarn_unused_function, [list/0, list/1, list/2, list/3, list/4, list/5, list/6, list/7, list/8, list/9, list/10, list/11, list/12, list/13, list/14, list/15, list/16, list/17, list/18, list/19, list/20, list/21, list/22, list/23, list/24, list/25, list/26]}).
-compile({nowarn_unused_function, [tuple/0, tuple/1, tuple/2, tuple/3, tuple/4, tuple/5, tuple/6, tuple/7, tuple/8, tuple/9, tuple/10, tuple/11, tuple/12, tuple/13, tuple/14, tuple/15, tuple/16, tuple/17, tuple/18, tuple/19, tuple/20, tuple/21, tuple/22, tuple/23, tuple/24, tuple/25, tuple/26]}).
-compile({nowarn_unused_function, [map/0, map/1, map/2, map/3, map/4, map/5, map/6, map/7, map/8, map/9, map/10, map/11, map/12, map/13, map/14, map/15, map/16, map/17, map/18, map/19, map/20, map/21, map/22, map/23, map/24, map/25, map/26]}).
-compile({nowarn_unused_function, [record/0, record/1, record/2, record/3, record/4, record/5, record/6, record/7, record/8, record/9, record/10, record/11, record/12, record/13, record/14, record/15, record/16, record/17, record/18, record/19, record/20, record/21, record/22, record/23, record/24, record/25, record/26]}).
%%%-------------------------------------------------------------------
%%% for sql/erl maker use
%%%-------------------------------------------------------------------
-record(sql, {
    insert,
    select,
    update,
    delete,
    except,
    duplicate,
    filter,
    into,
    from,
    join,
    use,
    by,
    group_by,
    having,
    order_by,
    limit,
    offset,
    return,
    as
}).

%% raw sql
raw(Raw) -> {'$raw$', Raw}.

%% binding param
param() -> {'$param$', []}.

%% https://mariadb.com/kb/en/window-functions-overview/
%% https://mariadb.com/kb/en/window-functions/

%% aggregate window functions
avg(Avg) -> {'$avg$', Avg}.
bit_and(BitAnd) -> {'$bit_and$', BitAnd}.
bit_or(BitOr) -> {'$bit_or$', BitOr}.
bit_xor(BitXor) -> {'$bit_xor$', BitXor}.
count(Count) -> {'$count$', Count}.
max(Max) -> {'$max$', Max}.
min(Min) -> {'$min$', Min}.
std(Std) -> {'$std$', Std}.
std_dev(StdDev) -> {'$std_dev$', StdDev}.
std_dev_pop(StdDevPop) -> {'$std_dev_pop$', StdDevPop}.
std_dev_sample(StdDevSample) -> {'$std_dev_sample$', StdDevSample}.
sum(Sum) -> {'$sum$', Sum}.
var_pop(VarPop) -> {'$var_pop$', VarPop}.
var_sample(VarSample) -> {'$var_sample$', VarSample}.
variance(Variance) -> {'$variance$', Variance}.

%%%-------------------------------------------------------------------
%%% for erl maker use
%%%-------------------------------------------------------------------

%% all
all(Data) -> {'$all$', Data}.

list() -> {'$list$', []}.
list(A) -> {'$list$', [A]}.
list(A, B) -> {'$list$', [A, B]}.
list(A, B, C) -> {'$list$', [A, B, C]}.
list(A, B, C, D) -> {'$list$', [A, B, C, D]}.
list(A, B, C, D, E) -> {'$list$', [A, B, C, D, E]}.
list(A, B, C, D, E, F) -> {'$list$', [A, B, C, D, E, F]}.
list(A, B, C, D, E, F, G) -> {'$list$', [A, B, C, D, E, F, G]}.
list(A, B, C, D, E, F, G, H) -> {'$list$', [A, B, C, D, E, F, G, H]}.
list(A, B, C, D, E, F, G, H, I) -> {'$list$', [A, B, C, D, E, F, G, H, I]}.
list(A, B, C, D, E, F, G, H, I, J) -> {'$list$', [A, B, C, D, E, F, G, H, I, J]}.
list(A, B, C, D, E, F, G, H, I, J, K) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K]}.
list(A, B, C, D, E, F, G, H, I, J, K, L) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y]}.
list(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) -> {'$list$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]}.

tuple() -> {'$tuple$', []}.
tuple(A) -> {'$tuple$', [A]}.
tuple(A, B) -> {'$tuple$', [A, B]}.
tuple(A, B, C) -> {'$tuple$', [A, B, C]}.
tuple(A, B, C, D) -> {'$tuple$', [A, B, C, D]}.
tuple(A, B, C, D, E) -> {'$tuple$', [A, B, C, D, E]}.
tuple(A, B, C, D, E, F) -> {'$tuple$', [A, B, C, D, E, F]}.
tuple(A, B, C, D, E, F, G) -> {'$tuple$', [A, B, C, D, E, F, G]}.
tuple(A, B, C, D, E, F, G, H) -> {'$tuple$', [A, B, C, D, E, F, G, H]}.
tuple(A, B, C, D, E, F, G, H, I) -> {'$tuple$', [A, B, C, D, E, F, G, H, I]}.
tuple(A, B, C, D, E, F, G, H, I, J) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J]}.
tuple(A, B, C, D, E, F, G, H, I, J, K) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y]}.
tuple(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) -> {'$tuple$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]}.

map() -> {'$map$', []}.
map(A) -> {'$map$', [A]}.
map(A, B) -> {'$map$', [A, B]}.
map(A, B, C) -> {'$map$', [A, B, C]}.
map(A, B, C, D) -> {'$map$', [A, B, C, D]}.
map(A, B, C, D, E) -> {'$map$', [A, B, C, D, E]}.
map(A, B, C, D, E, F) -> {'$map$', [A, B, C, D, E, F]}.
map(A, B, C, D, E, F, G) -> {'$map$', [A, B, C, D, E, F, G]}.
map(A, B, C, D, E, F, G, H) -> {'$map$', [A, B, C, D, E, F, G, H]}.
map(A, B, C, D, E, F, G, H, I) -> {'$map$', [A, B, C, D, E, F, G, H, I]}.
map(A, B, C, D, E, F, G, H, I, J) -> {'$map$', [A, B, C, D, E, F, G, H, I, J]}.
map(A, B, C, D, E, F, G, H, I, J, K) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K]}.
map(A, B, C, D, E, F, G, H, I, J, K, L) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y]}.
map(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) -> {'$map$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]}.

record() -> {'$record$', []}.
record(A) -> {'$record$', [A]}.
record(A, B) -> {'$record$', [A, B]}.
record(A, B, C) -> {'$record$', [A, B, C]}.
record(A, B, C, D) -> {'$record$', [A, B, C, D]}.
record(A, B, C, D, E) -> {'$record$', [A, B, C, D, E]}.
record(A, B, C, D, E, F) -> {'$record$', [A, B, C, D, E, F]}.
record(A, B, C, D, E, F, G) -> {'$record$', [A, B, C, D, E, F, G]}.
record(A, B, C, D, E, F, G, H) -> {'$record$', [A, B, C, D, E, F, G, H]}.
record(A, B, C, D, E, F, G, H, I) -> {'$record$', [A, B, C, D, E, F, G, H, I]}.
record(A, B, C, D, E, F, G, H, I, J) -> {'$record$', [A, B, C, D, E, F, G, H, I, J]}.
record(A, B, C, D, E, F, G, H, I, J, K) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K]}.
record(A, B, C, D, E, F, G, H, I, J, K, L) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y]}.
record(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z) -> {'$record$', [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z]}.

-endif.
