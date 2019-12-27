const cheatProtocol = {
    read: {
        60000: [
            {name: "result", type: "rst", comment: "结果", explain: []},
            {name: "command", type: "str", comment: "命令", explain: []}
        ]
    },
    write: {
        60000: [
            {name: "command", type: "str", comment: "命令", explain: []}
        ]
    }
};