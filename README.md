# server

##  **File Tree**
    |---beam                                          : beam  
    |---include                                       : include  
    |---logs                                          : application run log  
    |---config                                        : config  
        |---app                                       : app config  
        |---cert                                      : ssl cert  
    |---script                                        : script  
        |---batch                                     : windows batch script  
        |---shell                                     : linux/bash script  
        |---sql                                       : sql script  
        |---make                                      : maker and script  
            |---doc                                   : code maker doc  
            |---maker                                 : code maker  
            |---script                                : code maker script(manual)  
            |---protocol                              : protocol script (manual)  
                |---js                                : Js protocol metadata(auto make)  
                |---lua                               : Lua protocol metadata(auto make)  
    |---src                                           : src  
        |---lib                                       : third part dependency  
            |---mysql-connector-erlang                : MySQL connector  
            |---volley                                : Volley process pool  
            |---a_star                                : A* algorithm  
        |---application                               : application and service  
        |---net                                       : network I/O  
        |---tool                                      : common tool  
            |---assistant                             : framework assistant tool  
            |---extension                             : stdlib extension tool  
            |---misc                                  : misc tool  
        |---module                                    : game module  
            |---node                                  : node cluster manage tool  
            |---account                               : account module  
            |---user                                  : user service  
            |---role                                  : role module  
            |---asset                                 : asset module  
            |---item                                  : item module  
            |---task                                  : task module  
            |---shop                                  : shop module  
            |---mail                                  : mail module  
            |---friend                                : friend module  
            |---chat                                  : chat module  
            |---guild                                 : guild service  
            |---key                                   : active key service  
            |---notice                                : notice module  
            |---rank                                  : rank service  
            |---recharge                              : recharge module  
            |---activity                              : activity service  
            |---auction                               : auction service  
            |---attribute                             : attribute module  
            |---skill                                 : skill module  
            |---buff                                  : buff  
            |---battle                                : battle system  
            |---map                                   : map system  
            |---monster                               : monster manage  
            |---war                                   : war mange  
            |---dungeon                               : dungeon  
            |---count                                 : count  
            |---log                                   : game log  
            |---sorter                                : data sorter  
            |---effect                                : effect  
            |---increment                             : increment service  
            |---text                                  : text configure  
            |---parameter                             : customs parameter  
            |---robot                                 : robot  
            |---cheat                                 : cheat command  
            |---master                                : master command  

##  **Script Usage**
    1.script/batch/maker.bat  
    usage: maker.bat  
        debug [module]                                make (module) with debug mode  
        release [module]                              make (module) with release mode  
        clean                                         remove all beam  
        maker                                         compile maker  
        beam                                          update beam abstract code  
        pt name                                       make protocol file  
        protocol                                      make all protocol file  
        excel [table|xml] [table-name|file-name]      convert/restore table/xml to xml/table  
        xml table-name                                convert table to xml, same as excel xml table-name  
        table file-name                               restore xml to table, same as excel table file-name  
        record name                                   make record file  
        sql name                                      make sql file  
        data name                                     make erl data configure file  
        lua name                                      make lua data configure file  
        js name                                       make js data configure file  
        log name                                      make log file  
        word                                          make sensitive word file  
        key [-number|-type|-prefix]                   make active key  
        config                                        make erlang application config interface  
        router                                        make protocol route  
        loop                                          make load/save/reset/clean/expire code  
        attribute                                     make attribute code  
        asset                                         make asset code  
        helps                                         lookup help man  

    2.script/batch/run.bat  
    usage: run.bat  
        name                                          run config/name.config by interactive mode  

    3.script/shell/maker.sh  
    usage: maker.sh  
        debug [module]                                make (module) with debug mode  
        release [module]                              make (module) with release mode  
        clean                                         remove all beam  
        maker                                         compile maker  
        beam                                          update beam abstract code  
        now                                           append now to update sql script  
        tag                                           append tag to update sql script  
        need                                          cut last tag to end file, write to need sql script  
        need date(Y-M-D)                              cut from date(start) to now(end), write to need sql script  
        import [name]                                 import need sql to database, import to all database when the name not set  
        pt name                                       make protocol file  
        protocol                                      make all protocol file  
        excel [table|xml] [table-name|file-name]      convert/restore table/xml to xml/table  
        xml table-name                                convert table to xml, same as excel xml table-name  
        table file-name                               restore xml to table, same as excel table file-name  
        record name                                   make record file  
        sql name                                      make sql file  
        data name                                     make erl data configure file  
        lua name                                      make lua data configure file  
        js name                                       make js data configure file  
        log name                                      make log file  
        word                                          make sensitive word file  
        key [-number|-type|-prefix]                   make active key  
        config                                        make erlang application config interface  
        router                                        make protocol route  
        loop                                          make load/save/reset/clean/expire code  
        attribute                                     make attribute code  
        asset                                         make asset code  
        helps                                         lookup help man  

    4.script/shell/run.sh  
    usage: run.sh  
        name [bg|sh|stop]                             run/run detached/remote shell/stop node  
        [name|-] [-load|-force] modules ...           load modules on node/nodes  
        [name|-] -eval script                         execute script on node/nodes  
        [name|-] -sql [script]                        execute sql script on node/nodes  
        - start                                       start nodes  
        - stop                                        stop nodes  

    wildcard flag '-' can use node type restrict, such as:  
        run.sh -local -load ...  
        run.sh -center -eval ...  
        run.sh -world -sql ...  

##  **Request and Response**
    client request:  
        receiver -> *_protocol:read -> account -> user_server:socket_event -> *_handler:handle -> *:*  
    server response:  
        *:* -> *_handler:handle -> user_router:dispatch -> user_server:socket_event -> user_sender:send -> sender:send  
    progress detail:  
        receiver receive and handle packet data  
        *_protocol decode protocol packet  
        account handle protocol packet(packet control)  
        user_server receive and handle request  
        user_router/*_handler dispatch normal request  
        diff game module handle request and return to client  


##  **File Specs**
    item (user process)  
    path :  
        src/module/item/  
    files :  
        item.erl                   : data manage  
        item_data.erl              : configure  
        item_sql.erl               : database sync  
        item_handler.erl           : request handler  
        item_protocol.erl          : protocol encoder/decoder  

    active key (other process)  
    path :  
        src/module/key/  
    files :  
        key_server.erl             : gen_server process and data manage  
        key_data.erl               : configure  
        key_sql.erl                : database sync  
        key_handler.erl            : request handler  
        key_protocol.erl           : protocol encoder/decoder  

    guild (other process)  
    path :  
        src/module/guild/  
    files :  
        guild.erl                  : data manage  
        guild_server.erl           : gen_server process  
        guild_sql.erl              : guild database sync  
        guild_role_sql.erl         : guild role database sync  
        guild_apply_sql.erl        : guild apply database sync  
        guild_handler.erl          : request handler  
        guild_protocol.erl         : protocol encoder/decoder  

    use maker build and update code  
        maker.[bat/sh] record *    : *.hrl  
        maker.[bat/sh] sql *       : *_sql.erl  
        maker.[bat/sh] data *      : *_data.erl  
        maker.[bat/sh] pt *        : *_protocol.erl/*_handler.erl  
        maker.[bat/sh] log *       : log.erl/log_sql.erl  
        maker.[bat/sh] router      : user_router.erl  
        maker.[bat/sh] loop        : user_loop.erl  


##  **Code Specs**
    all file use utf8 without bom(byte order mark) encoding  
    line break use unix like LF(\n)  
    use four space replace Tab to align and indent  


##  **Database Specs**
    use Maria and InnoDB engine, characters set utf8mb4 and collation utf8mb4_unicode_ci  
    configure and normal table use Dynamic row format, log table use Compressed row format  
    integer type tiny(3)/small(5)/int(10)/big(20) unsigned default 0 not null  
    char/varchar default not null empty string  

##  **SQL Specs**
    1. table  
        configure first  *_data  
        data secondary  *  
        log at last  *_log  
    2. fields  
        update first  
        change secondary  
    3. data  
        add data first  
        revise data secondary  


##  **License**
`server` is licensed under [The GNU General Public License (GPL)](LICENSE).  
