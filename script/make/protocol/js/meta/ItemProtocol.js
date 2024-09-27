export default {
    "11106" : {
        "comment" : "使用物品",
        "write" : [
            {"name": "itemNo", "type": "u64", "comment": "物品编号", "explain": []},
            {"name": "number", "type": "u16", "comment": "数量", "explain": []},
            {"name": "type", "type": "u8", "comment": "类型", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    }
};