--- @class AuctionQueryRequest
--- @field protocol number 16101
--- @field data {
--- }

--- @class AuctionQueryRequest
--- @field protocol number 16101
--- @field data {
---     auctionNo: integer,                                                                         -- 拍品编号
---     auctionId: integer,                                                                         -- 拍品ID
---     number: integer,                                                                            -- 数量
---     type: integer,                                                                              -- 拍卖类型(1:全服/2:公会)
---     endTime: integer,                                                                           -- 结束时间
---     nowPrice: integer,                                                                          -- 当前价格
---     nextPrice: integer,                                                                         -- 下次出价的价格
--- }[]

--- @class AuctionBidRequest
--- @field protocol number 16102
--- @field data {
---     auctionNo: integer,                                                                         -- 拍品编号
---     nextPrice: integer,                                                                         -- 新的价格
--- }

--- @class AuctionBidRequest
--- @field protocol number 16102
--- @field data {
---     result: string,                                                                             -- 结果
---     newPrice: integer,                                                                          -- 新的价格
---     auction: {
---         auctionNo: integer,                                                                     -- 拍品编号
---         auctionId: integer,                                                                     -- 拍品ID
---         type: integer,                                                                          -- 拍卖类型(1:全服/2:公会)
---         endTime: integer,                                                                       -- 结束时间
---         nowPrice: integer,                                                                      -- 当前价格
---         nextPrice: integer,                                                                     -- 下次出价的价格
---     },                                                                                          -- 拍品
--- }

AuctionProtocol = {}

function AuctionProtocol.encode(offset, protocol, data)
    if protocol == 16101 then
        local table = {}

        return table
    elseif protocol == 16102 then
        local table = {}

        -- 拍品编号
        table[offset] = string.pack(">I8", data.auctionNo)
        offset = offset + 1
        -- 新的价格
        table[offset] = string.pack(">I4", data.nextPrice)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function AuctionProtocol.decode(offset, protocol, bytes)
    if protocol == 16101 then
        -- 拍品列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 拍品
            -- 拍品编号
            local dataDataAuctionNo = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 拍品ID
            local dataDataAuctionId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 数量
            local dataDataNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 拍卖类型(1:全服/2:公会)
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 结束时间
            local dataDataEndTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 当前价格
            local dataDataNowPrice = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 下次出价的价格
            local dataDataNextPrice = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {auctionNo = dataDataAuctionNo, auctionId = dataDataAuctionId, number = dataDataNumber, type = dataDataType, endTime = dataDataEndTime, nowPrice = dataDataNowPrice, nextPrice = dataDataNextPrice}
            data[dataIndex] = dataData
        end
        return {protocol = 16101, data = data}
    elseif protocol == 16102 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 新的价格
        local dataNewPrice = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 拍品
        -- 拍品编号
        local dataAuctionAuctionNo = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 拍品ID
        local dataAuctionAuctionId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 拍卖类型(1:全服/2:公会)
        local dataAuctionType = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 结束时间
        local dataAuctionEndTime = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 当前价格
        local dataAuctionNowPrice = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 下次出价的价格
        local dataAuctionNextPrice = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- object
        local dataAuction = {auctionNo = dataAuctionAuctionNo, auctionId = dataAuctionAuctionId, type = dataAuctionType, endTime = dataAuctionEndTime, nowPrice = dataAuctionNowPrice, nextPrice = dataAuctionNextPrice}
        -- object
        local data = {result = dataResult, newPrice = dataNewPrice, auction = dataAuction}
        return {protocol = 16102, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end