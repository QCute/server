# erlang

### 目录树说明
#####1. beam                          : beam 目录
#####2. include                       : 头文件目录
#####3. config                        : 配置目录
        |---main.app                  : 主程序配置
        |---main.config               : 应用程序运行配置 for sasl log
        |---debug_application.app     : 主程序配置(调试时使用)
        |---logs                      : sasl 日志目录
#####4. script                        : 脚本目录
            |---script                : windows 下使用
            |---shell                 : linux 下使用
#####5. src                           : 源代码目录
        |---main                      : 网络I/O
        |---data                      : 数值模板数据相关
            |---mysql                 : MySQL驱动
            |---poolboy               : PoolBoy进程池
            |---tool                  : 数据库相关工具
        |---protocol                  : 协议解包打包
        |---cluster                   : 集群相关工具
        |---tool                      : 通用工具
        |---debug                     : 调试工具相关
        |---make                      : 代码构造器目录
            |---maker                 : 代码构造器
            |---script                : 代码构造规则脚本(配置)
        |---example                   : Erlang Actor 示例代码
        |---module                    : 业务逻辑模块
            |---account               : 账户
            |---player                : 玩家
            |---item                  : 物品
            |---assess                : 资产
            |---quest                 : 任务
            |---shop                  : 商店
            |---mail                  : 邮件
            |---guild                 : 公会
            |---friend                : 好友
            |---chat                  : 聊天
            |---notice                : 公告
            |---rank                  : 排行
            |---log                   : 日志