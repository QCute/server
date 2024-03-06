## run script

#### script/shell/run.sh
```sh
usage: run.sh
    [name|-] [start|stop|state]                   start/stop node/node state
    name [interactive|shell]                      start with interactive mode/connect remote shell
    [name|-] [load|force] modules ...             load modules on node/nodes
    [name|-] [diff] [skip|true|false]             show module diff message
    [name|-] eval script                          execute script on node/nodes
    [name|-] [sql] [script|file|directory]        execute sql script/file/directory on node/nodes

wildcard flag '-' can use node type restrict, such as:
    run.sh -local load ...
    run.sh -center eval ...
    run.sh -world sql ...
```

## 部署

#### 交互模式启动
```sh
run.sh config_file_name interactive
```

#### 后台模式启动
```sh
run.sh config_file_name start
```

#### 进入后台启动服务的交互shell
```sh
run.sh config_file_name shell
```

#### 停止服务
```sh
run.sh config_file_name stop
```

## 热更新

#### 软加载代码
```sh
run.sh config_file_name load module1 module2 ...
```

#### 强制加载代码
```sh
run.sh config_file_name force module1 module2 ...
```

#### 查找代码差异
```sh
run.sh config_file_name diff true
```

## 数据库

#### 执行sql命令
```sh
run.sh config_file_name sql "SELECT 1"
```

#### 执行更新SQL脚本文件
```sh
run.sh config_file_name sql path/to/update.sql
```

#### 执行[script/sql/alpha](/script/sql/alpha)文件夹下所有更新SQL脚本文件
```sh
run.sh config_file_name sql alpha
```
