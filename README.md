## A Game Server Power by [Erlang](https://erlang.org)

##  **File Tree**
    ├── beam                   : beam  
    ├── include                : include  
    ├── logs                   : application run log  
    └── config                 : config  
        ├── app                : app config  
        ├── src                : config source  
        └── cert               : ssl cert  
    └── script                 : script  
        ├── batch              : windows batch script  
        ├── shell              : linux/bash script  
        ├── sql                : sql script  
        └── make               : code maker and script  
    ├── lib                    : third part dependency  
    └── src                    : src  
        ├── application        : application and service  
        ├── net                : network I/O  
        └── module             : game module  
            ├── user           : user service  
            ├── cheat          : cheat command  
            ├── master         : master command  
            └── ...            : other module
        └── tool               : common tool  
            ├── assistant      : framework assistant tool  
            ├── extension      : stdlib extension tool  
            └── misc           : misc tool  

##  **License**
`server` is licensed under [The GNU General Public License (GPL)](LICENSE).  
