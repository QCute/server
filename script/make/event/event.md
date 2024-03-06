## event script/maker 使用

#### 基本使用

* 在模块代码中编写
```erl
on_load(User, Event) ->
    Data = my_sql:select(User#user.role_id),
    User#user{my = Data}.
```

* 执行命令
```sh
mk event
```

* 上述命令会在`event`模块生成以下代码

```erl
trigger_static(User, Event = #event{name = load}) ->
    ...
    my:on_load(PreviousUser, Event),
    ...
```

#### 不使用事件数据

* 在模块代码中编写
```erl
on_load(User) ->
    Data = my_sql:select(User#user.role_id),
    User#user{my = Data}.
```

* 执行命令
```sh
mk event
```

* 在`event`模块生成以下代码

```erl
trigger_static(User, Event = #event{name = load}) ->
    ...
    my:on_load(PreviousUser),
    ...
```
