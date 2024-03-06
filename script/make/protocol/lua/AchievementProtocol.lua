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
        table[offset] = string.pack(">I4", data["achievementId"])
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
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 统计类型
            local type = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 总数
            local totalNumber = string.unpack(">I4", data, offset)
            offset = offset + 4
            list[listIndex] = {type = type, totalNumber = totalNumber}
        end
        return {list = list}
    elseif protocol == 12202 then
        local offset = offset
        -- 成就列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 成就ID
            local achievementId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 成就类型
            local type = string.unpack(">I4", data, offset)
            offset = offset + 4
            list[listIndex] = {achievementId = achievementId, type = type}
        end
        return {list = list}
    elseif protocol == 12203 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end