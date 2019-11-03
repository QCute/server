local auctionProtocol = {
    ["read"] = {
        [16101] = {
            {name = "list", type = "list", comment = "拍品列表", explain = {
                {name = "uniqueId", type = "u64", comment = "唯一ID", explain = {}},
                {name = "auctionId", type = "u32", comment = "拍品ID", explain = {}},
                {name = "number", type = "u16", comment = "数量", explain = {}},
                {name = "endTime", type = "u32", comment = "结束时间", explain = {}},
                {name = "price", type = "u32", comment = "价格", explain = {}},
                {name = "bidderId", type = "u64", comment = "竞拍者", explain = {}},
                {name = "bidderName", type = "bst", comment = "竞拍者名", explain = {}}
            }}
        },
        [16102] = {
            {name = "result", type = "u8", comment = "结果", explain = {}},
            {name = "newPrice", type = "u32", comment = "新的价格", explain = {}},
            {name = "uniqueId", type = "u64", comment = "唯一ID", explain = {}},
            {name = "auctionId", type = "u32", comment = "拍品ID", explain = {}},
            {name = "number", type = "u16", comment = "数量", explain = {}},
            {name = "endTime", type = "u32", comment = "结束时间", explain = {}},
            {name = "price", type = "u32", comment = "价格", explain = {}},
            {name = "bidderId", type = "u64", comment = "竞拍者", explain = {}},
            {name = "bidderName", type = "bst", comment = "竞拍者名", explain = {}}
        }
    },
    ["write"] = {
        [16101] = {},
        [16102] = {
            {name = "uniqueId", type = "u64", comment = "唯一ID", explain = {}}
        }
    }
}