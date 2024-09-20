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
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Shop
            -- 商店ID
            local shopId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local shop = {shopId = shopId, number = number}
            list[listIndex] = shop
        end
        return {list = list}
    elseif protocol == 11302 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end