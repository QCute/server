function encodeShopProtocol(offset, protocol, data)
    local switch = {
        [11302] = function()
            local offset = offset
            local table = {}
            -- 商店ID
            table[offset] = string.pack(">I4", data["shopId"])
            offset = offset + 1
            -- 数量
            table[offset] = string.pack(">I2", data["number"])
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

function decodeShopProtocol(offset, protocol, data)
    local switch = {
        [11301] = function()
            local offset = offset
            -- 已购买列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 商店ID
                local shopId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local number = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {shopId = shopId, number = number}
            end
            return {list = list}
        end,
        [11302] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end