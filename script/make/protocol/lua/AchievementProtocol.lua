AchievementProtocol = {}

function AchievementProtocol.encode(offset, protocol, data)
    if protocol == 12201 then
        local table = {}

        return table
    elseif protocol == 12202 then
        local table = {}

        return table
    elseif protocol == 12203 then
        local table = {}
        -- 成就ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function AchievementProtocol.decode(offset, protocol, bytes)
    if protocol == 12201 then
        -- 统计列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 统计类型
            local dataDataType = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 总数
            local dataDataTotalNumber = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {type = dataDataType, totalNumber = dataDataTotalNumber}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 12202 then
        -- 成就列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 成就ID
            local dataDataAchievementId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 成就类型
            local dataDataType = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {achievementId = dataDataAchievementId, type = dataDataType}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 12203 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end