# maker

* script/batch/maker.bat  
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
    loop                                          make load/save/reset/clean/expire code
    attribute                                     make attribute code
    asset                                         make asset code
    event                                         make event code
    helps                                         lookup help manual
```


## 构建相关

* Debug模式编译
```sh
maker.sh debug module1, module2...
```

* Release模式编译
```sh
maker.sh release module1, module2...
```

* 编译Maker代码
```sh
maker.sh maker
```

* 编译Lib代码
```sh
maker.sh lib
```

* 清除编译代码
```sh
maker.sh clean
```

## 开发辅助相关

* 生成record beam代码
```sh
maker.sh beam
```

* 生成plt代码
```sh
maker.sh plt
```

* 分析代码
```sh
maker.sh dialyzer
```

* 生成excel(多个表)
```sh
maker.sh book
```

* 生成excel(单个表)
```sh
maker.sh sheet
```

* 导入excel(多个表)
```sh
maker.sh collection
```

* 导入excel(单个表)
```sh
maker.sh table
```

## 代码生成相关

* 生成record代码
```sh
maker.sh record name
```

* 生成sql代码
```sh
maker.sh sql name
```

* 生成erl代码
```sh
maker.sh erl name
```

* 生成lua代码
```sh
maker.sh lua name
```

* 生成js代码
```sh
maker.sh js name
```

* 生成log代码
```sh
maker.sh log
```

* 生成协议代码
```sh
maker.sh pt name
```

* 生成Router代码
```sh
maker.sh router
```

* 生成Loop代码
```sh
maker.sh loop
```

* 生成Config代码
```sh
maker.sh config
```
