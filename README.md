# erlang

### **文件目录树说明**
    |---beam                          : beam 文件目录  
    |---include                       : 头文件目录
    |---logs                          : 程序运行日志目录
    |---config                        : 配置目录  
        |---cert                      : ssl证书目录  
    |---script                        : 脚本目录  
        |---batch                     : windows 下使用  
        |---shell                     : linux 下使用  
        |---sql                       : sql脚本
        |---debug                     : Debug 模式 Makefile
        |---release                   : Release 模式 Makefile
    |---src                           : 源代码目录  
        |---application               : 应用目录
        |---service                   : 应用服务目录
        |---net                       : 网络I/O  
        |---protocol                  : 协议解包打包  
        |---cluster                   : 集群相关工具  
        |---tool                      : 通用工具  
        |---debug                     : 调试工具相关  
        |---robot                     : 机器人  
        |---example                   : Erlang Actor 示例代码  
        |---data                      : 数值模板数据  
        |---lib                       : 第三方依赖库  
            |---mysql                 : MySQL驱动  
            |---poolboy               : PoolBoy进程池  
        |---make                      : 代码构造器目录  
            |---maker                 : 代码构造器  
            |---script                : 代码构造规则脚本(配置)  
            |---protocol              : 协议代码构造规则脚本(配置)
        |---module                    : 业务逻辑模块  
            |---account               : 账户  
            |---player                : 玩家  
            |---item                  : 物品  
            |---quest                 : 任务  
            |---shop                  : 商店  
            |---mail                  : 邮件  
            |---key                   : 兑换码  
            |---guild                 : 公会  
            |---relation              : 关系(好友/黑名单) 
            |---chat                  : 聊天  
            |---notice                : 公告  
            |---rank                  : 排行  
            |---sorter                : 排序器  
            |---log                   : 数据日志  
            |---attribute             : 属性  
            |---skill                 : 技能  
            |---buff                  : Buff  
            |---battle                : 战斗系统  
            |---map                   : 地图
            |---monster               : 怪物
            |---war                   : 战场  
            |---dungeon               : 副本  
            |---activity              : 活动  


### **脚本说明**
    1.script/batch/maker.bat
    usage: compile all file by default  
        clean                                     remove all beam  
        maker                                     compile maker  
        pt/protocol number                        make protocol file  
        excel [xml|table] [filename|table name]   convert xml/table to table/xml  
        record name                               make record file  
        sql name [select|join] [all]              make sql file  
        data name                                 make base data config file  
        log name                                  make log file  
        word                                      make sensitive word file  
        key [-amount|-type|-prefix]               make active key  
        config                                    make erlang application config interface  

    2.script/batch/run.bat
    usage: run program (main config by default)  
        name                                      run config/name.config by interactive mode

    3.script/shell/maker.sh
    usage: compile all file by default
        clean                                     remove all beam  
        maker                                     compile maker  
        now                                       append now to update sql script  
        need date(Y-M-D)                          cut from date(start) to now(end), write to need sql script  
        pt/protocol number                        make protocol file  
        excel [xml|table] [filename|table name]   convert xml/table to table/xml  
        record name                               make record file  
        sql name [select|join] [all]              make sql file  
        data name                                 make base data config file  
        lua name                                  make lua data config file  
        log name                                  make log file  
        word                                      make sensitive word file  
        key [-amount|-type|-prefix]               make active key  
        config                                    make erlang application config interface  

    4.script/shell/run.sh
    usage: run program (run all config dir config file by bg mode if name not passed)
        name [bg | remsh | load]                  run config/name.config by mode

