SkillProtocol = {}

function SkillProtocol.encode(offset, protocol, data)
    if protocol == 11701 then
        local table = {}

        return table
    elseif protocol == 11702 then
        local table = {}
        -- 技能ID
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function SkillProtocol.decode(offset, protocol, bytes)
    if protocol == 11701 then
        -- 技能列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 技能ID
            local dataDataSkillId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 技能等级
            local dataDataLevel = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {skillId = dataDataSkillId, level = dataDataLevel}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 11702 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end