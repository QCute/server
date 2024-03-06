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
        table[offset] = string.pack(">I4", data)
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
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 技能ID
            local skillId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 技能等级
            local level = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local skill = {skillId = skillId, level = level}
            data[dataIndex] = skill
        end
        return data
    elseif protocol == 11702 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end