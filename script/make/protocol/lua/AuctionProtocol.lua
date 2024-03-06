AuctionProtocol = {}

function AuctionProtocol.encode(offset, protocol, data)
    if protocol == 16101 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 16102 then
        local offset = offset
        local table = {}
        -- 拍品编号
        table[offset] = string.pack(">I8", data["auctionNo"])
        offset = offset + 1
        -- 新的价格
        table[offset] = string.pack(">I4", data["nextPrice"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function AuctionProtocol.decode(offset, protocol, data)
    if protocol == 16101 then
        local offset = offset
        -- 拍品列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 拍品
            -- 拍品编号
            local auctionNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 拍品ID
            local auctionId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 拍卖类型(1:全服/2:公会)
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 结束时间
            local endTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 当前价格
            local nowPrice = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 下次出价的价格
            local nextPrice = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local auction = {auctionNo = auctionNo, auctionId = auctionId, number = number, type = type, endTime = endTime, nowPrice = nowPrice, nextPrice = nextPrice}
            data[dataIndex] = auction
        end
        return data
    elseif protocol == 16102 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 新的价格
        local newPrice = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 拍品
        -- 拍品编号
        local auctionAuctionNo = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 拍品ID
        local auctionAuctionId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 拍卖类型(1:全服/2:公会)
        local auctionType = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 结束时间
        local auctionEndTime = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 当前价格
        local auctionNowPrice = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 下次出价的价格
        local auctionNextPrice = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- object
        local auction = {auctionNo = auctionAuctionNo, auctionId = auctionAuctionId, type = auctionType, endTime = auctionEndTime, nowPrice = auctionNowPrice, nextPrice = auctionNextPrice}
        -- object
        local data = {result = result, newPrice = newPrice, auction = auction}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end