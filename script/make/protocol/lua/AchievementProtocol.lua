function encodeAchievementProtocol(offset, protocol, data)
    local switch = {
        [12203] = function()
            local offset = offset
            local table = {}
            -- 成就ID
            table[offset] = string.pack(">I4", data["achievementId"])
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

function decodeAchievementProtocol(offset, protocol, data)
    local switch = {
        [12301] = function()
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
        end,
        [12202] = function()
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
        end,
        [12203] = function()
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