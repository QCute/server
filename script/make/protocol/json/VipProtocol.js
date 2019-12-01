const vipProtocol = {
    read: {
        10301: [
            {name: "vipLevel", type: "u8", comment: "等级", explain: []},
            {name: "exp", type: "u64", comment: "经验", explain: []},
            {name: "expireTime", type: "u32", comment: "过期时间", explain: []}
        ]
    },
    write: {
        10301: []
    }
};