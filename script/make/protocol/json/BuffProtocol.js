const buffProtocol = {
    "write" : {
        "11801" : []
    },
    "read" : {
        "11801" : [
            {"name" : "list", "type" : "list", "comment" : "Buff列表", "explain" : [
                {"name" : "buffId", "type" : "u32", "comment" : "BuffID", "explain" : []},
                {"name" : "expireTime", "type" : "u32", "comment" : "结束时间", "explain" : []},
                {"name" : "overlap", "type" : "u16", "comment" : "叠加数量", "explain" : []}
            ]}
        ],
        "11802" : [
            {"name" : "list", "type" : "list", "comment" : "Buff列表", "explain" : [
                {"name" : "buffId", "type" : "u32", "comment" : "BuffID", "explain" : []}
            ]}
        ]
    }
};