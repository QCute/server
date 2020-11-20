# server

##  **文件目录树说明**
    |---beam                                      : beam 文件目录  
    |---include                                   : 头文件目录  
    |---logs                                      : 程序运行日志目录  
    |---config                                    : 配置目录  
        |---app                                   : 应用目录  
        |---cert                                  : ssl证书目录  
    |---script                                    : 脚本目录  
        |---batch                                 : windows 下使用  
        |---shell                                 : linux 下使用  
        |---sql                                   : sql脚本  
        |---make                                  : 代码构造器目录  
            |---doc                               : 代码构造脚本使用文档  
            |---maker                             : 代码构造器  
            |---script                            : 代码构造规则脚本(配置)  
            |---protocol                          : 协议代码构造规则脚本(配置)  
                |---js                            : Js协议元数据目录(生成)  
                |---lua                           : Lua协议元数据目录(生成)  
    |---src                                       : 源代码目录  
        |---lib                                   : 第三方依赖库  
            |---mysql-connector-erlang            : MySQL连接器  
            |---volley                            : Volley进程池  
            |---a_star                            : A*算法  
        |---application                           : 应用程序目录  
        |---net                                   : 网络I/O  
        |---tool                                  : 通用工具  
            |---assistant                         : 框架辅助工具  
            |---extension                         : 标准库扩展工具  
            |---misc                              : 其他各种各样的/杂乱的工具  
        |---module                                : 业务逻辑模块  
            |---node                              : 节点集群工具  
            |---account                           : 账户  
            |---user                              : 玩家  
            |---role                              : 角色  
            |---asset                             : 资产  
            |---item                              : 物品  
            |---quest                             : 任务  
            |---shop                              : 商店  
            |---mail                              : 邮件  
            |---friend                            : 好友  
            |---chat                              : 聊天  
            |---guild                             : 公会  
            |---key                               : 兑换码  
            |---notice                            : 公告  
            |---rank                              : 排行榜  
            |---recharge                          : 充值  
            |---activity                          : 活动  
            |---auction                           : 拍卖  
            |---attribute                         : 属性  
            |---skill                             : 技能  
            |---buff                              : 状态增益  
            |---battle                            : 战斗系统  
            |---map                               : 地图系统  
            |---monster                           : 怪物  
            |---war                               : 战场  
            |---dungeon                           : 副本  
            |---count                             : 计数  
            |---log                               : 日志  
            |---sorter                            : 排序器  
            |---effect                            : 作用效果  
            |---increment                         : 自增  
            |---text                              : 文本数据  
            |---parameter                         : 自定义参数  
            |---robot                             : 机器人  
            |---cheat                             : 作弊命令  
            |---master                            : 管理员命令  

##  **脚本说明**
    1.script/batch/maker.bat  
    usage: compile all file by default  
        clean                                     remove all beam  
        maker                                     compile maker  
        pt/protocol number                        make protocol file  
        excel [xml|table] [filename|table name]   convert xml/table to table/xml  
        xml table-name                            convert table to xml  
        table  file-name                          restore xml to table  
        record name                               make record file  
        sql name [select|join] [all]              make sql file  
        data name                                 make erl data configure file  
        lua name                                  make lua data configure file  
        js name                                   make js data configure file  
        log name                                  make log file  
        word                                      make sensitive word file  
        key [-number|-type|-prefix]               make active key  
        config                                    make erlang application config interface  
        router                                    maker protocol route  
        loop                                      maker load/save/reset/clean/expire code  

    2.script/batch/runner.bat  
    usage: run program (main config by default)  
        name                                      run config/name.config by interactive mode  

    3.script/shell/maker.sh  
    usage: compile all file by default  
        clean                                     remove all beam  
        maker                                     compile maker  
        now                                       append now to update sql script  
        need date(Y-M-D)                          cut from date(start) to now(end), write to need sql script  
        pt name                                   make protocol file  
        protocol                                  make all protocol file  
        excel [xml|table] [filename|table name]   convert xml/table to table/xml  
        xml table-name                            convert table to xml  
        table  file-name                          restore xml to table  
        record name                               make record file  
        sql name [select|join] [all]              make sql file  
        data name                                 make erl data configure file  
        lua name                                  make lua data configure file  
        js name                                   make js data configure file  
        log name                                  make log file  
        word                                      make sensitive word file  
        key [-number|-type|-prefix]               make active key  
        config                                    make erlang application config interface  
        router                                    maker protocol route  
        loop                                      maker load/save/reset/clean/expire code  

    4.script/shell/runner.sh  
    usage: run program (run all config dir config file by bg mode if name not passed)  
        name [bg | sh | stop]                     run/stop/remote shell config/name.config by mode  
        [name | =] [load | force] modules,...     load modules on node/nodes by load mode  
        [name | =] evaluate "script"              execute script on node/nodes  
        +                                         start all  
        -                                         stop all  

##  **数据流具体说明**
    请求走向:  
        receiver -> *_protocol:read -> account -> user_server:socket_event -> *_handler:handle -> *:*  
    答复走向:  
        *:* -> *_handler:handle -> user_router:dispatch -> user_server:socket_event -> user_sender:send -> sender:send  
    流程:  
        receiver 接收并处理数据  
        *_protocol 解析协议内容  
        account 处理封包数据  
        user_server 接收玩家数据  
        user_router 分发处理  
        *_handler 分发具体功能类型处理  
        具体模块处理完返回数据在user_server函数处理并返回客户端  
    

##  **功能文件放置说明**
    例如物品 (玩家进程)  
    路径 :  
        src/module/item/  
    包含文件 :  
        item.erl                   : 物品数据操作模块  
        item_data.erl              : 物品数值配置表模块  
        item_sql.erl               : 玩家物品数据SQL模块  
        item_handler.erl           : 上行协议处理模块  
        item_protocol.erl          : 协议打包解包模块  

    例如兑换码 (单独进程)  
    路径 :  
        src/module/key/  
    包含文件 :  
        key_server.erl             : 兑换码数据操作和进程模块  
        key_data.erl               : 兑换码数值配置表模块  
        key_sql.erl                : 玩家兑换码数据SQL模块  
        key_handler.erl            : 上行协议处理模块  
        key_protocol.erl           : 协议打包解包模块  

    例如公会 (单独进程)  
    路径 :  
        src/module/guild/  
    包含文件 :  
        guild.erl                  : 公会数据操作模块  
        guild_server.erl           : 公会进程模块  
        guild_sql.erl              : 公会数据SQL模块  
        guild_role_sql.erl         : 公会玩家数据SQL模块  
        guild_apply_sql.erl        : 公会申请数据SQL模块  
        guild_handler.erl          : 上行协议处理模块  
        guild_protocol.erl         : 协议打包解包模块  

    使用代码构造器maker构建/更新代码  
        maker.[bat/sh] record *    : *.hrl  
        maker.[bat/sh] sql *       : *_sql.erl  
        maker.[bat/sh] data *      : *_data.erl  
        maker.[bat/sh] pt *        : *_protocol.erl/*_handler.erl  
        maker.[bat/sh] log *       : log.erl/log_sql.erl  
        maker.[bat/sh] router      : user_router.erl  
        maker.[bat/sh] loop        : user_loop.erl  


##  **代码文件要求**
    编码使用utf8 no bom(byte order mark)  
    换行符使用unix like 的LF(\n)  
    使用四个空格替换Tab进行缩进与对齐  
    目录/模块/变量/函数等命名禁止使用中文拼音或者拼音首字母  
    单词拼写检测和单词缩写以Intellij Idea Typo为准，词库参考https://github.com/LibreOffice/dictionaries/blob/master/en/en_US.dic  


##  **数据库要求**
    使用MariaDB InnoDB引擎, 字符集utf8mb4和校对规则utf8mb4_unicode_ci(unicode为德/法/俄语等校验)  
    配置表和数据表使用Dynamic行格式,日志表使用Compressed行格式  
    整型tiny(3)/small(5)/int(10)/big(20) 默认为0非空且无符号(unsigned)  
    char/varchar 默认为空字符串且非空  

##  **更新SQL放置要求**
    同个版本自己的SQL语句放在一起(同个版本开发时间内后续修改)  
    同个版本的新建/修改表,增/改字段直接改动原来的建表语句,不需要再单独放置增/改语句  
    1. 表类  
        首先配置表  *_data  
        其次数据表  *  
        然后日志表  *_log  
    2. 字段类  
        放置更改字段语句  
    3. 数据类  
        最后放置数据增加/修正语句  


##  **目标**
    程序员以偷懒为天职  
    能不手写的代码就不手写  

