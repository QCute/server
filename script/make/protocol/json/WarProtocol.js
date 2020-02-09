const warProtocol = {
    read: {
        18001: [
            {name: "result", type: "rst", comment: "结果", explain: []}
        ]
    },
    write: {
        18001: [
            {name: "monsterId", type: "u32", comment: "怪物Id", explain: []}
        ]
    }
};