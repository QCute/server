DungeonProtocol = {}

function DungeonProtocol.encode(offset, protocol, data)
    if protocol == 17001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 17002 then
        local offset = offset
        local table = {}
        -- 副本Id
        table[offset] = string.pack(">I4", data["dungeonId"])
        offset = offset + 1
        return table
    elseif protocol == 17005 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function DungeonProtocol.decode(offset, protocol, data)
    if protocol == 17001 then
        local offset = offset
        -- 
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Dungeon
            -- 副本Id
            local dungeonId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 今天次数
            local todayNumber = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 总次数
            local totalNumber = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local dungeon = {dungeonId = dungeonId, todayNumber = todayNumber, totalNumber = totalNumber}
            list[listIndex] = dungeon
        end
        return {list = list}
    elseif protocol == 17002 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 17003 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 17004 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 17005 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end