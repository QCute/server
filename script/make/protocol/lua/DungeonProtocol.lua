function encodeDungeonProtocol(offset, protocol, data)
    local switch = {
        [17002] = function()
            local offset = offset
            local table = {}
            -- 副本Id
            table[offset] = string.pack(">I4", data["dungeonId"])
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

function decodeDungeonProtocol(offset, protocol, data)
    local switch = {
        [17001] = function()
            local offset = offset
            -- 
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 副本Id
                local dungeonId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 今天次数
                local todayNumber = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- 总次数
                local totalNumber = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {dungeonId = dungeonId, todayNumber = todayNumber, totalNumber = totalNumber}
            end
            return {list = list}
        end,
        [17002] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [17003] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [17004] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [17005] = function()
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