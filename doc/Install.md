
## 拉取仓库
```sh
git clone https://github.com:QCute/server
```

## 拉取子模块仓库
```
git submodule update --recursive
```

## 使用配置 `config/src/{type}.config.src`
```sh
cp config/src/local.config.src config/local.config
```

## 更改数据库配置
```erl
        {mysql_connector, [                                                                        %% database connector config
            {host,                                    "127.0.0.1"},                                %% database address
            {port,                                    3306},                                       %% database port
            {user,                                    "root"},                                     %% database user
            {password,                                "root"},                                     %% database password
            {database,                                "local"},                                    %% database name
            {encoding,                                "utf8mb4"}                                   %% database encoding
        ]},
```

## 构建
```sh
./script/shell/maker release
```

* or

```bat
./script/batch/maker.bat release
```

## 运行
```sh
./script/shell/run.sh interactive
```

* or

```bat
./script/batch/run.bat
```
