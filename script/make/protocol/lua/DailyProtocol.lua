--- @class DailyQueryActiveRequest
--- @field protocol number 12301
--- @field data {
--- }

--- @class DailyQueryActiveRequest
--- @field protocol number 12301
--- @field data {
---     stageId: integer,                                                                           -- 奖励阶段ID
---     score: integer,                                                                             -- 活跃度
--- }

--- @class DailyQueryRequest
--- @field protocol number 12302
--- @field data {
--- }

--- @class DailyQueryRequest
--- @field protocol number 12302
--- @field data {
---     dailyId: integer,                                                                           -- 日常ID
---     isAward: integer,                                                                           -- 是否领取奖励
--- }[]

--- @class DailyAwardRequest
--- @field protocol number 12303
--- @field data integer

--- @class DailyAwardRequest
--- @field protocol number 12303
--- @field data string

--- @class DailyAwardActiveRequest
--- @field protocol number 12304
--- @field data integer

--- @class DailyAwardActiveRequest
--- @field protocol number 12304
--- @field data string

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
        -- 奖励阶段ID
        local dataStageId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 活跃度
        local dataScore = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- object
        local data = {stageId = dataStageId, score = dataScore}
        return {protocol = 12301, data = data}
    elseif protocol == 12302 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 日常ID
            local dataDataDailyId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 是否领取奖励
            local dataDataIsAward = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataData = {dailyId = dataDataDailyId, isAward = dataDataIsAward}
            data[dataIndex] = dataData
        end
        return {protocol = 12302, data = data}
    elseif protocol == 12303 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 12303, data = data}
    elseif protocol == 12304 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 12304, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end