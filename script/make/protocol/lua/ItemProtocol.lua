--- @class ItemQueryItemRequest
--- @field protocol number 11101
--- @field data {
--- }

--- @class ItemQueryItemRequest
--- @field protocol number 11101
--- @field data {
---     itemNo: integer,                                                                            -- 物品编号
---     itemId: integer,                                                                            -- 物品ID
---     type: integer,                                                                              -- 类型
---     number: integer,                                                                            -- 数量
--- }[]

--- @class ItemQueryBagRequest
--- @field protocol number 11102
--- @field data {
--- }

--- @class ItemQueryBagRequest
--- @field protocol number 11102
--- @field data {
---     itemNo: integer,                                                                            -- 物品编号
---     itemId: integer,                                                                            -- 物品ID
---     type: integer,                                                                              -- 类型
---     number: integer,                                                                            -- 数量
--- }[]

--- @class ItemQueryStoreRequest
--- @field protocol number 11103
--- @field data {
--- }

--- @class ItemQueryStoreRequest
--- @field protocol number 11103
--- @field data {
---     itemNo: integer,                                                                            -- 物品编号
---     itemId: integer,                                                                            -- 物品ID
---     type: integer,                                                                              -- 类型
---     number: integer,                                                                            -- 数量
--- }[]



--- @class ItemDeleteRequest
--- @field protocol number 11104
--- @field data {
---     itemNo: integer,                                                                            -- 物品编号
---     itemId: integer,                                                                            -- 物品ID
---     type: integer,                                                                              -- 类型
--- }[]

--- @class ItemUseRequest
--- @field protocol number 11106
--- @field data {
---     itemNo: integer,                                                                            -- 物品编号
---     number: integer,                                                                            -- 数量
---     type: integer,                                                                              -- 类型
--- }

--- @class ItemUseRequest
--- @field protocol number 11106
--- @field data string

ItemProtocol = {}

function ItemProtocol.encode(offset, protocol, data)
    if protocol == 11101 then
        local table = {}

        return table
    elseif protocol == 11102 then
        local table = {}

        return table
    elseif protocol == 11103 then
        local table = {}

        return table
    elseif protocol == 11106 then
        local table = {}

        -- 物品编号
        table[offset] = string.pack(">I8", data.itemNo)
        offset = offset + 1
        -- 数量
        table[offset] = string.pack(">I2", data.number)
        offset = offset + 1
        -- 类型
        table[offset] = string.pack(">I1", data.type)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function ItemProtocol.decode(offset, protocol, bytes)
    if protocol == 11101 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local dataDataItemNo = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 物品ID
            local dataDataItemId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 数量
            local dataDataNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {itemNo = dataDataItemNo, itemId = dataDataItemId, type = dataDataType, number = dataDataNumber}
            data[dataIndex] = dataData
        end
        return {protocol = 11101, data = data}
    elseif protocol == 11102 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local dataDataItemNo = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 物品ID
            local dataDataItemId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 数量
            local dataDataNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {itemNo = dataDataItemNo, itemId = dataDataItemId, type = dataDataType, number = dataDataNumber}
            data[dataIndex] = dataData
        end
        return {protocol = 11102, data = data}
    elseif protocol == 11103 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local dataDataItemNo = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 物品ID
            local dataDataItemId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 数量
            local dataDataNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {itemNo = dataDataItemNo, itemId = dataDataItemId, type = dataDataType, number = dataDataNumber}
            data[dataIndex] = dataData
        end
        return {protocol = 11103, data = data}
    elseif protocol == 11104 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 物品编号
            local dataDataItemNo = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 物品ID
            local dataDataItemId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataData = {itemNo = dataDataItemNo, itemId = dataDataItemId, type = dataDataType}
            data[dataIndex] = dataData
        end
        return {protocol = 11104, data = data}
    elseif protocol == 11106 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 11106, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end