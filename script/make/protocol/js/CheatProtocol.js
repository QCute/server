const cheatProtocol = {
    "60001" : {
        "comment" : "秘籍",
        "write" : [],
        "read" : [
            {"name" : "cheatList", "type" : "list", "comment" : "秘籍列表", "explain" : [
                {"name" : "description", "type" : "str", "comment" : "描述", "explain" : []},
                {"name" : "command", "type" : "str", "comment" : "命令", "explain" : []}
            ]}
        ]
    },
    "60002" : {
        "comment" : "秘籍",
        "write" : [
            {"name" : "command", "type" : "str", "comment" : "命令", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};