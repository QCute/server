# volley
* a lightweight erlang process pool inspired from pool-boy, erl-pool, cue-sport, revolver
* simple and fast
* easy to integrate in your project

# quick start
* Add to rebar.config
```
{deps, [
  ...
  {volley, {git, "https://github.com/QCute/volley.git", {branch, "master"}}}
]}.
```

* start a pool  
1. define a worker  

```
-module(test).
-export([start_link/0]).
start_link() ->
    loop().

loop() ->
    receive
        Msg ->
            io:format("receive: ~p~n", [Msg]),
            loop()
    end.
```

2. start worker pool  

```
PoolArgs = [{worker, {test, start_link, []}}, {size, 4}],
volley:start_pool(a_pool, PoolArgs).
```

3. get a worker  

```
{ok, Worker} = volley:get(a_pool),
%% do something ...
```
