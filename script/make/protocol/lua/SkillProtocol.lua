SkillProtocol = {}

function SkillProtocol.encode(offset, protocol, data)
    if protocol == 11701 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11702 then
        local offset = offset
        local table = {}
        -- 技能ID
        table[offset] = string.pack(">I4", data["skillId"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function SkillProtocol.decode(offset, protocol, data)
    if protocol == 11701 then
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
    elseif protocol == 11702 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end