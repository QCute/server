const mapProtocol = {
    read: {
        20001: [
            {name: "list", type: "list", comment: "对象列表", explain: [
                {name: "id", type: "u64", comment: "ID", explain: []},
                {name: "type", type: "u8", comment: "类型", explain: []},
                {name: "x", type: "u16", comment: "X坐标", explain: []},
                {name: "y", type: "u16", comment: "Y坐标", explain: []}
            ]}
        ],
        20007: [
            {name: "id", type: "u64", comment: "ID", explain: []},
            {name: "x", type: "u16", comment: "X坐标", explain: []},
            {name: "y", type: "u16", comment: "Y坐标", explain: []}
        ],
        20008: [
            {name: "id", type: "u64", comment: "ID", explain: []},
            {name: "x", type: "u16", comment: "X坐标", explain: []},
            {name: "y", type: "u16", comment: "Y坐标", explain: []}
        ]
    },
    write: {
        20001: [],
        20007: [
            {name: "x", type: "u16", comment: "X坐标", explain: []},
            {name: "y", type: "u16", comment: "Y坐标", explain: []}
        ],
        20008: []
    }
};