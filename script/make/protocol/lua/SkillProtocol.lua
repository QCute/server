function encodeSkillProtocol(offset, protocol, data)
    local switch = {
        [11702] = function()
            local offset = offset
            local table = {}
            -- 技能ID
            table[offset] = string.pack(">I4", data["skillId"])
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

function decodeSkillProtocol(offset, protocol, data)
    local switch = {
        [11701] = function()
            local offset = offset
            -- 技能列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 技能ID
                local skillId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 技能等级
                local level = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {skillId = skillId, level = level}
            end
            return {list = list}
        end,
        [11702] = function()
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