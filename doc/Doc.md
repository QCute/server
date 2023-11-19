# 服务器文档

- [Install](/doc/Install.md) 安装文档
- [User](/doc/USER.md) 玩家数据模块基本使用.
- [Server](/doc/SERVER.md) 单独服务模块基本使用.
- [SQL File](/script/sql/sql.md) SQL脚本文件放置与使用说明.
- [Maker.sh](/script/shell/doc/maker.md), [Maker.bat](/script/batch/doc/maker.md) 构建工具文档.
- [Run.sh](/script/shell/doc/run.md), [Run.bat](/script/batch/doc/run.md) 运维工具文档.
- [Record](/script/make/record/record.md) 数据库表与记录映射生成工具.
- [SQL](/script/make/sql/sql.md) ORM工具.
- [Erl](/script/make/erl/erl.md) Erl配置数据生成工具.
- [Lua](/script/make/lua/lua.md) Lua配置数据生成工具.
- [Js](/script/make/js/js.md) Js配置数据生成工具.
- [Protocol](/script/make/protocol/protocol.md) 协议编码/解码生成工具.
- [Excel](/script/make/excel/excel.md) 数据库与excel文件导入导出工具.
- [Log](/script/make/log/log.md) 业务数据日志接口生成工具.
- [Event](/script/make/event/event.md) 事件接口生成工具.

####  **文件夹说明**
    ├── beam                   : beam文件  
    ├── include                : 头文件  
    ├── logs                   : 程序日志  
    └── config                 : 程序配置  
        ├── app                : app配置  
        ├── src                : 配置源  
        └── cert               : SSL证书  
    └── script                 : 脚本  
        ├── batch              : Windows批处理脚本  
        ├── shell              : Linux Bash脚本  
        ├── sql                : SQL脚本  
        └── make               : 代码生成脚本  
    ├── lib                    : 第三方依赖  
    └── src                    : 源文件  
        ├── application        : 程序和服务  
        ├── net                : 网络 I/O  
        └── module             : 游戏模块  
            ├── user           : 玩家模块  
            ├── cheat          : 秘籍命令  
            ├── master         : 管理员命令  
            └── ...            : 其他模块
        └── tool               : 工具  
            ├── assistant      : 助手工具  
            ├── extension      : 标准库扩展工具  
            └── misc           : 杂乱工具  
