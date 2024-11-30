## A Game Server Power by [Erlang](https://erlang.org)

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
        |---make                                      : code maker and script  
    |---lib                                           : third part dependency  
    |---src                                           : src  
        |---application                               : application and service  
        |---net                                       : network I/O  
        |---module                                    : game module  
            |---user                                  : user service  
            |---cheat                                 : cheat command  
            |---master                                : master command  
            |---...                                   : other module
        |---tool                                      : common tool  
            |---assistant                             : framework assistant tool  
            |---extension                             : stdlib extension tool  
            |---misc                                  : misc tool  

##  **Request and Response**
1. Client request:  
    1. receiver, receive packet data
    2. *_protocol:decode, decode packet data
    3. account, send request data to user server
    4. user_server:socket_event, route dispatch
    5. *_handler:handle, module scope route dispatch
    6. module:function, handle request data and return response
2. Server response:  
    1. module:function, return response
    2. user_server after dispatch, encode response data and send
    5. user_sender:send, wait to send packet data
    6. sender:send, socket send implement


##  **Code Specs**
1. All file use utf8 without bom(byte order mark) encoding  
2. Line break use unix like LF(\n)  
3. Use four space replace Tab to align and indent  


##  **Database Specs**
1. Use `MariaDB` and `InnoDB` engine, characters set `utf8mb4` and collation `utf8mb4_unicode_ci`  
2. Setup `configure` and `data` table use **Dynamic** row format, `log` table use **Compressed** row format  
3. Column `integer` type tiny(3)/small(5)/int(10)/big(20) **UNSIGNED** default 0 and **NOT NULL**
4. Column `char`/`varchar` default empty string and **NOT NULL**   

##  **SQL Specs**
1. Table  
    1. Configure first  *_data  
    2. Configure test  *_test_data  
    3. Data secondary  *  
    4. Log at last  *_log  
2. Fields  
    1. Update first  
    2. Change secondary  
3. Data  
    1. Add data first  
    2. Revise data secondary  


##  **License**
`server` is licensed under [The GNU General Public License (GPL)](LICENSE).  
