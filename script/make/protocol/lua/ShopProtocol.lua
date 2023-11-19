ShopProtocol = {}

function ShopProtocol.encode(offset, protocol, data)
    if protocol == 11301 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11302 then
        local offset = offset
        local table = {}
        -- 商店ID
        table[offset] = string.pack(">I4", data["shopId"])
        offset = offset + 1
        -- 数量
        table[offset] = string.pack(">I2", data["number"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function ShopProtocol.decode(offset, protocol, data)
    if protocol == 11301 then
        local offset = offset
        -- 已购买列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 商店ID
            local shopId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local shop = {shopId = shopId, number = number}
            data[dataIndex] = shop
        end
        return data
    elseif protocol == 11302 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end