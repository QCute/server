const itemProtocol = {
    read: {
        11101: [
            {name: "list", type: "list", comment: "道具列表", explain: [
                {name: "itemNo", type: "u64", comment: "物品编号", explain: []},
                {name: "itemId", type: "u32", comment: "物品ID", explain: []},
                {name: "type", type: "u8", comment: "类型", explain: []},
                {name: "number", type: "u16", comment: "数量", explain: []}
            ]}
        ],
        11102: [
            {name: "list", type: "list", comment: "背包列表", explain: [
                {name: "itemNo", type: "u64", comment: "物品编号", explain: []},
                {name: "itemId", type: "u32", comment: "物品ID", explain: []},
                {name: "type", type: "u8", comment: "类型", explain: []},
                {name: "number", type: "u16", comment: "数量", explain: []}
            ]}
        ],
        11103: [
            {name: "list", type: "list", comment: "仓库列表", explain: [
                {name: "itemNo", type: "u64", comment: "物品编号", explain: []},
                {name: "itemId", type: "u32", comment: "物品ID", explain: []},
                {name: "type", type: "u8", comment: "类型", explain: []},
                {name: "number", type: "u16", comment: "数量", explain: []}
            ]}
        ],
        11104: [
            {name: "list", type: "list", comment: "删除列表", explain: [
                {name: "itemNo", type: "u64", comment: "物品编号", explain: []},
                {name: "type", type: "u8", comment: "类型", explain: []}
            ]}
        ]
    },
    write: {
        11101: [],
        11102: [],
        11103: []
    }
};