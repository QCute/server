## maker script

#### script/batch/maker.bat
```bat
usage: maker.bat
    debug [module]                                make (module) with debug mode
    release [module]                              make (module) with release mode
    maker                                         compile maker
    lib                                           compile lib
    beam                                          update beam abstract code
    clean                                         remove all beam
    plt                                           make .dialyzer_plt file
    dialyzer                                      run dialyzer
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
```bat
maker.bat debug module1, module2...
```

#### Release模式编译
```bat
maker.bat release module1, module2...
```

#### 编译Maker代码
```bat
maker.bat maker
```

#### 编译Lib代码
```bat
maker.bat lib
```

#### 清除编译代码
```bat
maker.bat clean
```

## 开发辅助相关

#### 为[user_default.erl](/src/tool/extension/user_default.erl)文件生成全部头文件导入语句
```bat
# user_default.erl
# ...
# -include("../../../include/common.hrl").
# ...
maker.bat beam
```

#### 生成plt代码
```bat
# 生成文件 ~/.dialyzer.plt
maker.bat plt
```

#### 分析代码
```bat
maker.bat dialyzer
```

#### 生成excel(多个表), [使用文档](/script/make/excel/excel.md)
```bat
# 生成文件 comment.xlsm
maker.bat book file
```

#### 生成excel(单个表), [使用文档](/script/make/excel/excel.md)
```bat
# 生成文件 comment.xlsm
maker.bat sheet table
```

#### 导入excel(多个表), [使用文档](/script/make/excel/excel.md)
```bat
# 使用 comment.xlsm 导入到 table, table2 ...
maker.bat collection comment.xlsm
```

#### 导入excel(单个表), [使用文档](/script/make/excel/excel.md)
```bat
# 使用 comment.xlsm 导入到 table
maker.bat table comment.xlsm
```

## 代码生成相关

#### 生成record代码, [使用文档](/script/make/record/record.md)
```bat
maker.bat record name
```

#### 生成sql代码, [使用文档](/script/make/sql/sql.md)
```bat
maker.bat sql name
```

#### 生成erl代码, [使用文档](/script/make/erl/erl.md)
```bat
maker.bat erl name
```

#### 生成lua代码, [使用文档](/script/make/lua/lua.md)
```bat
maker.bat lua name
```

#### 生成js代码, [使用文档](/script/make/js/js.md)
```bat
maker.bat js name
```

#### 生成协议代码, [使用文档](/script/make/protocol/protocol.md)
```bat
maker.bat pt name
```

#### 生成log代码, [使用文档](/script/make/log/log.md)
```bat
maker.bat log
```

#### 生成Config代码
```bat
maker.bat config
```
