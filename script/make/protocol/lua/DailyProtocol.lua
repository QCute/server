DailyProtocol = {}

function DailyProtocol.encode(offset, protocol, data)
    if protocol == 12301 then
        local table = {}

        return table
    elseif protocol == 12302 then
        local table = {}

        return table
    elseif protocol == 12303 then
        local table = {}
        -- 日常ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    elseif protocol == 12304 then
        local table = {}
        -- 阶段ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function DailyProtocol.decode(offset, protocol, bytes)
    if protocol == 12301 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 统计类型
            local dataDataType = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 今日数量
            local dataDataTodayNumber = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {type = dataDataType, todayNumber = dataDataTodayNumber}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 12302 then
        -- 
        -- 
        -- 日常ID
        local dataDailyDailyId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 是否领取奖励
        local dataDailyIsAward = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- object
        local dataDaily = {dailyId = dataDailyDailyId, isAward = dataDailyIsAward}
        -- 
        -- 奖励阶段ID
        local dataDailyActiveStageId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 活跃度
        local dataDailyActiveScore = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- object
        local dataDailyActive = {stageId = dataDailyActiveStageId, score = dataDailyActiveScore}
        -- object
        local data = {daily = dataDaily, dailyActive = dataDailyActive}
        return data
    elseif protocol == 12303 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 12304 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end