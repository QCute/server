DailyProtocol = {}

function DailyProtocol.encode(offset, protocol, data)
    if protocol == 12301 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 12302 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 12303 then
        local offset = offset
        local table = {}
        -- 日常ID
        table[offset] = string.pack(">I4", data["dailyId"])
        offset = offset + 1
        return table
    elseif protocol == 12304 then
        local offset = offset
        local table = {}
        -- 阶段ID
        table[offset] = string.pack(">I4", data["stageId"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function DailyProtocol.decode(offset, protocol, data)
    if protocol == 12301 then
        local offset = offset
        -- 统计列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Count
            -- 统计类型
            local type = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 今日数量
            local todayNumber = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local count = {type = type, todayNumber = todayNumber}
            list[listIndex] = count
        end
        return {list = list}
    elseif protocol == 12302 then
        local offset = offset
        -- 日常列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Daily
            -- 日常ID
            local dailyId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 是否领取奖励
            local isAward = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local daily = {dailyId = dailyId, isAward = isAward}
            list[listIndex] = daily
        end
        -- DailyActive
        -- 奖励阶段ID
        local stageId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 活跃度
        local score = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- object
        local dailyActive = {stageId = stageId, score = score}
        return {list = list, dailyActive = dailyActive}
    elseif protocol == 12303 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 12304 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end