export default {
    "11101" : {
        "comment" : "道具列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "道具列表", "explain" : [
                {"name" : "itemNo", "type" : "u64", "comment" : "物品编号", "explain" : []},
                {"name" : "itemId", "type" : "u32", "comment" : "物品ID", "explain" : []},
                {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []},
                {"name" : "number", "type" : "u16", "comment" : "数量", "explain" : []}
            ]}
        ]
    },
    "11102" : {
        "comment" : "背包列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "背包列表", "explain" : [
                {"name" : "itemNo", "type" : "u64", "comment" : "物品编号", "explain" : []},
                {"name" : "itemId", "type" : "u32", "comment" : "物品ID", "explain" : []},
                {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []},
                {"name" : "number", "type" : "u16", "comment" : "数量", "explain" : []}
            ]}
        ]
    },
    "11103" : {
        "comment" : "仓库列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "仓库列表", "explain" : [
                {"name" : "itemNo", "type" : "u64", "comment" : "物品编号", "explain" : []},
                {"name" : "itemId", "type" : "u32", "comment" : "物品ID", "explain" : []},
                {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []},
                {"name" : "number", "type" : "u16", "comment" : "数量", "explain" : []}
            ]}
        ]
    },
    "11104" : {
        "comment" : "删除物品",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "删除列表", "explain" : [
                {"name" : "itemNo", "type" : "u64", "comment" : "物品编号", "explain" : []},
                {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []}
            ]}
        ]
    },
    "11106" : {
        "comment" : "使用物品",
        "write" : [
            {"name" : "itemNo", "type" : "u64", "comment" : "物品编号", "explain" : []},
            {"name" : "number", "type" : "u16", "comment" : "数量", "explain" : []},
            {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};