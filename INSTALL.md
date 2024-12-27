
## Clone repository
```sh
git clone https://github.com:QCute/server
```

## Clone sub repository
```
git submodule update --recursive
```

## Setup config from `config/src/{type}.config.src`
```sh
cp config/src/local.config.src config/local.config
```

## Change database connection
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

## Build
```sh
./script/shell/maker release
```

* or

```bat
./script/batch/maker.bat release
```

## Run
```sh
./script/shell/run.sh interactive
```

* or

```bat
./script/batch/run.bat
```
