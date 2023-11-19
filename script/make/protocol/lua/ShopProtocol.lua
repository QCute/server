--- @class ShopQueryRequest
--- @field protocol number 11301
--- @field data {
--- }

--- @class ShopQueryRequest
--- @field protocol number 11301
--- @field data {
---     shopId: integer,                                                                            -- 商店ID
---     number: integer,                                                                            -- 数量
--- }[]

--- @class ShopBuyRequest
--- @field protocol number 11302
--- @field data {
---     shopId: integer,                                                                            -- 商店ID
---     number: integer,                                                                            -- 数量
--- }

--- @class ShopBuyRequest
--- @field protocol number 11302
--- @field data string

ShopProtocol = {}

function ShopProtocol.encode(offset, protocol, data)
    if protocol == 11301 then
        local table = {}

        return table
    elseif protocol == 11302 then
        local table = {}

        -- 商店ID
        table[offset] = string.pack(">I4", data.shopId)
        offset = offset + 1
        -- 数量
        table[offset] = string.pack(">I2", data.number)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function ShopProtocol.decode(offset, protocol, bytes)
    if protocol == 11301 then
        -- 已购买列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 商店ID
            local dataDataShopId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 数量
            local dataDataNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {shopId = dataDataShopId, number = dataDataNumber}
            data[dataIndex] = dataData
        end
        return {protocol = 11301, data = data}
    elseif protocol == 11302 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 11302, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end