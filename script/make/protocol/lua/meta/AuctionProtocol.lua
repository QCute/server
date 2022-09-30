auctionProtocol = {
    [16101] = {
        ["comment"] = "拍品列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "拍品列表", explain = {
                {name = "auctionNo", type = "u64", comment = "拍品编号", explain = {}},
                {name = "auctionId", type = "u32", comment = "拍品ID", explain = {}},
                {name = "number", type = "u16", comment = "数量", explain = {}},
                {name = "type", type = "u8", comment = "拍卖类型(1:全服/2:公会)", explain = {}},
                {name = "endTime", type = "u32", comment = "结束时间", explain = {}},
                {name = "nowPrice", type = "u32", comment = "当前价格", explain = {}},
                {name = "nextPrice", type = "u32", comment = "下次出价的价格", explain = {}}
            }}
        }
    },
    [16102] = {
        ["comment"] = "竞价",
        ["write"] = {
            {name = "auctionNo", type = "u64", comment = "拍品编号", explain = {}},
            {name = "nextPrice", type = "u32", comment = "新的价格", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}},
            {name = "newPrice", type = "u32", comment = "新的价格", explain = {}},
            {name = "auctionNo", type = "u64", comment = "拍品编号", explain = {}},
            {name = "auctionId", type = "u32", comment = "拍品ID", explain = {}},
            {name = "type", type = "u8", comment = "拍卖类型(1:全服/2:公会)", explain = {}},
            {name = "endTime", type = "u32", comment = "结束时间", explain = {}},
            {name = "nowPrice", type = "u32", comment = "当前价格", explain = {}},
            {name = "nextPrice", type = "u32", comment = "下次出价的价格", explain = {}}
        }
    }
}