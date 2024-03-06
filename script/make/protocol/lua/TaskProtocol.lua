TaskProtocol = {}

function TaskProtocol.encode(offset, protocol, data)
    if protocol == 11201 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11202 then
        local offset = offset
        local table = {}
        -- 任务ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    elseif protocol == 11203 then
        local offset = offset
        local table = {}
        -- 任务ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TaskProtocol.decode(offset, protocol, data)
    if protocol == 11201 then
        local offset = offset
        -- 任务列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 任务ID
            local taskId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 当前数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 是否领取奖励
            local isAward = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local task = {taskId = taskId, number = number, isAward = isAward}
            data[dataIndex] = task
        end
        return data
    elseif protocol == 11202 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 
        -- 任务ID
        local taskTaskId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 当前数量
        local taskNumber = string.unpack(">I2", data, offset)
        offset = offset + 2
        -- 是否领取奖励
        local taskIsAward = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- object
        local task = {taskId = taskTaskId, number = taskNumber, isAward = taskIsAward}
        -- object
        local data = {result = result, task = task}
        return data
    elseif protocol == 11203 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end