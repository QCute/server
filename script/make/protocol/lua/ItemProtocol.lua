function encodeItemProtocol(offset, protocol, data)
    local switch = {
        [11106] = function()
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
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeItemProtocol(offset, protocol, data)
    local switch = {
        [11101] = function()
            local offset = offset
            -- 道具列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 物品编号
                local itemNo = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 物品ID
                local itemId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 类型
                local type = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 数量
                local number = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {itemNo = itemNo, itemId = itemId, type = type, number = number}
            end
            return {list = list}
        end,
        [11102] = function()
            local offset = offset
            -- 背包列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 物品编号
                local itemNo = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 物品ID
                local itemId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 类型
                local type = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 数量
                local number = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {itemNo = itemNo, itemId = itemId, type = type, number = number}
            end
            return {list = list}
        end,
        [11103] = function()
            local offset = offset
            -- 仓库列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 物品编号
                local itemNo = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 物品ID
                local itemId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 类型
                local type = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 数量
                local number = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {itemNo = itemNo, itemId = itemId, type = type, number = number}
            end
            return {list = list}
        end,
        [11104] = function()
            local offset = offset
            -- 删除列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 物品编号
                local itemNo = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 类型
                local type = string.unpack(">I1", data, offset)
                offset = offset + 1
                list[listIndex] = {itemNo = itemNo, type = type}
            end
            return {list = list}
        end,
        [11106] = function()
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