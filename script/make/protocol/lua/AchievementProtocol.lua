AchievementProtocol = {}

function AchievementProtocol.encode(offset, protocol, data)
    if protocol == 12301 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 12202 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 12203 then
        local offset = offset
        local table = {}
        -- 成就ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function AchievementProtocol.decode(offset, protocol, data)
    if protocol == 12301 then
        local offset = offset
        -- 统计列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 统计类型
            local type = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 总数
            local totalNumber = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local count = {type = type, totalNumber = totalNumber}
            data[dataIndex] = count
        end
        return data
    elseif protocol == 12202 then
        local offset = offset
        -- 成就列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 成就ID
            local achievementId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 成就类型
            local type = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local achievement = {achievementId = achievementId, type = type}
            data[dataIndex] = achievement
        end
        return data
    elseif protocol == 12203 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end