DungeonProtocol = {}

function DungeonProtocol.encode(offset, protocol, data)
    if protocol == 17001 then
        local table = {}

        return table
    elseif protocol == 17002 then
        local table = {}
        -- 副本Id
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    elseif protocol == 17005 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function DungeonProtocol.decode(offset, protocol, bytes)
    if protocol == 17001 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 副本Id
            local dataDataDungeonId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 今天次数
            local dataDataTodayNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 总次数
            local dataDataTotalNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {dungeonId = dataDataDungeonId, todayNumber = dataDataTodayNumber, totalNumber = dataDataTotalNumber}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 17002 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 17003 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 17004 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 17005 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end