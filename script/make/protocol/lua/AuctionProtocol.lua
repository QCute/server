function encodeAuctionProtocol(offset, protocol, data)
    local switch = {
        [16102] = function()
            local offset = offset
            local table = {}
            -- 拍品编号
            table[offset] = string.pack(">I8", data["auctionNo"])
            offset = offset + 1
            -- 新的价格
            table[offset] = string.pack(">I4", data["nextPrice"])
            offset = offset + 1
            return table
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeAuctionProtocol(offset, protocol, data)
    local switch = {
        [16101] = function()
            local offset = offset
            -- 拍品列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
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
                list[listIndex] = {auctionNo = auctionNo, auctionId = auctionId, number = number, type = type, endTime = endTime, nowPrice = nowPrice, nextPrice = nextPrice}
            end
            return {list = list}
        end,
        [16102] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 新的价格
            local newPrice = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 拍品编号
            local auctionNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 拍品ID
            local auctionId = string.unpack(">I4", data, offset)
            offset = offset + 4
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
            return {result = result, newPrice = newPrice, auctionNo = auctionNo, auctionId = auctionId, type = type, endTime = endTime, nowPrice = nowPrice, nextPrice = nextPrice}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end