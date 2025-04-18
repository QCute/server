## maker script

#### script/shell/maker.sh  
```sh
usage: maker.sh
    debug [module]                                make (module) with debug mode
    release [module]                              make (module) with release mode
    maker                                         compile maker
    lib                                           compile lib
    beam                                          update beam abstract code
    clean                                         remove all beam
    plt                                           make .dialyzer_plt file
    dialyzer                                      run dialyzer
    version                                       make version
    now                                           append now to update sql script
    tag                                           append tag to update sql script
    migrate                                       cut last tag to end file, write to migrate sql script
    migrate date(Y-M-D)                           cut from date(start) to now(end), write to migrate sql script
    pt name                                       make protocol file
    protocol                                      make all protocol file
    book file-name                                convert tables to excel book
    sheet table-name                              convert table to excel sheet, same as excel table-name
    collection file-name                          restore book file to tables
    table file-name                               restore sheet file to table, same as excel table file-name
    record name                                   make record file
    sql name                                      make sql file
    erl name                                      make erl data configure file
    lua name                                      make lua data configure file
    js name                                       make js data configure file
    log                                           make log file
    word                                          make sensitive word file
    key [-number|-type|-prefix]                   make active key
    config                                        make erlang application config interface
    router                                        make protocol route
    attribute                                     make attribute code
    asset                                         make asset code
    event                                         make event code
    helps                                         lookup help manual
```


## 构建相关

#### Debug模式编译
```sh
maker.sh debug module1, module2...
```

#### Release模式编译
```sh
maker.sh release module1, module2...
```

#### 编译Maker代码
```sh
maker.sh maker
```

#### 编译Lib代码
```sh
maker.sh lib
```

#### 清除编译代码
```sh
maker.sh clean
```

## 开发辅助相关

#### 为[user_default.erl](/src/tool/extension/user_default.erl)文件生成全部头文件导入语句
```sh
# user_default.erl
# ...
# -include("../../../include/common.hrl").
# ...
maker.sh beam
```

#### 生成plt代码
```sh
# 生成文件 ~/.dialyzer.plt
maker.sh plt
```

#### 分析代码
```sh
maker.sh dialyzer
```

#### 生成excel(多个表), [使用文档](/script/make/excel/excel.md)
```sh
# 生成文件 comment.xlsm
maker.sh book file
```

#### 生成excel(单个表), [使用文档](/script/make/excel/excel.md)
```sh
# 生成文件 comment.xlsm
maker.sh sheet table
```

#### 导入excel(多个表), [使用文档](/script/make/excel/excel.md)
```sh
# 使用 comment.xlsm 导入到 table, table2 ...
maker.sh collection comment.xlsm
```

#### 导入excel(单个表), [使用文档](/script/make/excel/excel.md)
```sh
# 使用 comment.xlsm 导入到 table
maker.sh table comment.xlsm
```

## 代码生成相关

#### 生成record代码, [使用文档](/script/make/record/record.md)
```sh
maker.sh record name
```

#### 生成sql代码, [使用文档](/script/make/sql/sql.md)
```sh
maker.sh sql name
```

#### 生成erl代码, [使用文档](/script/make/erl/erl.md)
```sh
maker.sh erl name
```

#### 生成lua代码, [使用文档](/script/make/lua/lua.md)
```sh
maker.sh lua name
```

#### 生成js代码, [使用文档](/script/make/js/js.md)
```sh
maker.sh js name
```

#### 生成协议代码, [使用文档](/script/make/protocol/protocol.md)
```sh
maker.sh pt name
```

#### 生成log代码, [使用文档](/script/make/log/log.md)
```sh
maker.sh log
```

#### 生成Config代码
```sh
maker.sh config
```
