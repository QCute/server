function encodeTaskProtocol(offset, protocol, data)
    local switch = {
        [11202] = function()
            local offset = offset
            local table = {}
            -- 任务ID
            table[offset] = string.pack(">I4", data["taskId"])
            offset = offset + 1
            return table
        end,
        [11203] = function()
            local offset = offset
            local table = {}
            -- 任务ID
            table[offset] = string.pack(">I4", data["taskId"])
            offset = offset + 1
            return table
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeTaskProtocol(offset, protocol, data)
    local switch = {
        [11201] = function()
            local offset = offset
            -- 任务列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 任务ID
                local taskId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 当前数量
                local number = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- 是否领取奖励
                local isAward = string.unpack(">I1", data, offset)
                offset = offset + 1
                list[listIndex] = {taskId = taskId, number = number, isAward = isAward}
            end
            return {list = list}
        end,
        [11202] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 任务ID
            local taskId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 当前数量
            local number = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 是否领取奖励
            local isAward = string.unpack(">I1", data, offset)
            offset = offset + 1
            return {result = result, taskId = taskId, number = number, isAward = isAward}
        end,
        [11203] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end