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
1. All file use **utf8** without bom(byte order mark) encoding  
2. Line break use **unix** like LF(\n)  
3. Use 4 **space** replace **tab** to align and indent  


##  **License**
`server` is licensed under [The GNU General Public License (GPL)](LICENSE).  
