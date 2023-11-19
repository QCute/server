--- @class TaskQueryRequest
--- @field protocol number 11201
--- @field data {
--- }

--- @class TaskQueryRequest
--- @field protocol number 11201
--- @field data {
---     taskId: integer,                                                                            -- 任务ID
---     number: integer,                                                                            -- 当前数量
---     isAward: integer,                                                                           -- 是否领取奖励
--- }[]

--- @class TaskAcceptRequest
--- @field protocol number 11202
--- @field data integer

--- @class TaskAcceptRequest
--- @field protocol number 11202
--- @field data {
---     result: string,                                                                             -- 结果
---     task: {
---         taskId: integer,                                                                        -- 任务ID
---         number: integer,                                                                        -- 当前数量
---         isAward: integer,                                                                       -- 是否领取奖励
---     },                                                                                          -- 
--- }

--- @class TaskSubmitRequest
--- @field protocol number 11203
--- @field data integer

--- @class TaskSubmitRequest
--- @field protocol number 11203
--- @field data string

TaskProtocol = {}

function TaskProtocol.encode(offset, protocol, data)
    if protocol == 11201 then
        local table = {}

        return table
    elseif protocol == 11202 then
        local table = {}
        -- 任务ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    elseif protocol == 11203 then
        local table = {}
        -- 任务ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TaskProtocol.decode(offset, protocol, bytes)
    if protocol == 11201 then
        -- 任务列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 任务ID
            local dataDataTaskId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 当前数量
            local dataDataNumber = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 是否领取奖励
            local dataDataIsAward = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataData = {taskId = dataDataTaskId, number = dataDataNumber, isAward = dataDataIsAward}
            data[dataIndex] = dataData
        end
        return {protocol = 11201, data = data}
    elseif protocol == 11202 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 
        -- 任务ID
        local dataTaskTaskId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 当前数量
        local dataTaskNumber = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        -- 是否领取奖励
        local dataTaskIsAward = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- object
        local dataTask = {taskId = dataTaskTaskId, number = dataTaskNumber, isAward = dataTaskIsAward}
        -- object
        local data = {result = dataResult, task = dataTask}
        return {protocol = 11202, data = data}
    elseif protocol == 11203 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 11203, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end