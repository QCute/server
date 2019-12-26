const noticeProtocol = {
    read: {
        50001: [
            {name: "scope", type: "u8", comment: "范围", explain: []},
            {name: "type", type: "u8", comment: "类型", explain: []},
            {name: "msg", type: "bst", comment: "消息", explain: []}
        ]
    },
    write: {

    }
};