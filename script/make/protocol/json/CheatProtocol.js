const cheatProtocol = {
    read: {
        60001: [
            {name: "cheatList", type: "list", comment: "秘籍列表", explain: [
                {name: "description", type: "str", comment: "描述", explain: []},
                {name: "command", type: "str", comment: "命令", explain: []}
            ]}
        ],
        60002: [
            {name: "result", type: "rst", comment: "结果", explain: []}
        ]
    },
    write: {
        60001: [],
        60002: [
            {name: "command", type: "str", comment: "命令", explain: []}
        ]
    }
};