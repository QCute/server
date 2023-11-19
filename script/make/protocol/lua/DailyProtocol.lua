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
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    elseif protocol == 12304 then
        local offset = offset
        local table = {}
        -- 阶段ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function DailyProtocol.decode(offset, protocol, data)
    if protocol == 12301 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 统计类型
            local type = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 今日数量
            local todayNumber = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local count = {type = type, todayNumber = todayNumber}
            data[dataIndex] = count
        end
        return data
    elseif protocol == 12302 then
        local offset = offset
        -- 
        -- 
        -- 日常ID
        local dailyDailyId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 是否领取奖励
        local dailyIsAward = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- object
        local daily = {dailyId = dailyDailyId, isAward = dailyIsAward}
        -- 
        -- 奖励阶段ID
        local dailyActiveStageId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 活跃度
        local dailyActiveScore = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- object
        local dailyActive = {stageId = dailyActiveStageId, score = dailyActiveScore}
        -- object
        local data = {daily = daily, dailyActive = dailyActive}
        return data
    elseif protocol == 12303 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 12304 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end