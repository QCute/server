const bubbleProtocol = {
    "write" : {
        "12101" : []
    },
    "read" : {
        "12101" : [
            {"name" : "list", "type" : "list", "comment" : "气泡列表", "explain" : [
                {"name" : "bubbleId", "type" : "u32", "comment" : "气泡ID", "explain" : []},
                {"name" : "expireTime", "type" : "u32", "comment" : "过期时间", "explain" : []}
            ]}
        ],
        "12102" : [
            {"name" : "list", "type" : "list", "comment" : "气泡ID列表", "explain" : [
                {"name" : "bubbleId", "type" : "u32", "comment" : "气泡ID", "explain" : []}
            ]}
        ]
    }
};