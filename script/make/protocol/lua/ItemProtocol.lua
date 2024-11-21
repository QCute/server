ItemProtocol = {}

function ItemProtocol.encode(offset, protocol, data)
    if protocol == 11101 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11102 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11103 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11106 then
        local offset = offset
        local table = {}
        -- 物品编号
        table[offset] = string.pack(">I8", data["itemNo"])
        offset = offset + 1
        -- 数量
        table[offset] = string.pack(">I2", data["number"])
        offset = offset + 1
        -- 类型
        table[offset] = string.pack(">I1", data["type"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function ItemProtocol.decode(offset, protocol, data)
    if protocol == 11101 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local itemNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 物品ID
            local itemId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local item = {itemNo = itemNo, itemId = itemId, type = type, number = number}
            data[dataIndex] = item
        end
        return data
    elseif protocol == 11102 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local itemNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 物品ID
            local itemId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local item = {itemNo = itemNo, itemId = itemId, type = type, number = number}
            data[dataIndex] = item
        end
        return data
    elseif protocol == 11103 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local itemNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 物品ID
            local itemId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local item = {itemNo = itemNo, itemId = itemId, type = type, number = number}
            data[dataIndex] = item
        end
        return data
    elseif protocol == 11104 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local itemNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 物品ID
            local itemId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local item = {itemNo = itemNo, itemId = itemId, type = type}
            data[dataIndex] = item
        end
        return data
    elseif protocol == 11106 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end