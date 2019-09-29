let secretProtocol = {
    "read" : {
        "60000" : [
            {"name" : "command", "type" : "str", "comment" : "命令", "explain" : []}
        ]
    },
    "write" : {
        "60000" : [
            {"name" : "result", "type" : "u8", "comment" : "结果(0:失败/1:成功)", "explain" : []},
            {"name" : "command", "type" : "str", "comment" : "命令", "explain" : []}
        ]
    }
};