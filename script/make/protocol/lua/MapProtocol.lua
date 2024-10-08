MapProtocol = {}

function MapProtocol.encode(offset, protocol, data)
    if protocol == 20001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 20012 then
        local offset = offset
        local table = {}
        -- X坐标
        table[offset] = string.pack(">I2", data["data"]["x"])
        offset = offset + 1
        -- Y坐标
        table[offset] = string.pack(">I2", data["data"]["y"])
        offset = offset + 1
        return table
    elseif protocol == 20014 then
        local offset = offset
        local table = {}
        -- 技能Id
        table[offset] = string.pack(">I4", data["data"]["skillId"])
        offset = offset + 1
        -- 战斗对象ID列表
        local targetListTable = data["data"]["targetList"]
        table[offset] = string.pack(">I2", #targetListTable)
        offset = offset + 1
        for targetListIndex = 1, #targetListTable do
            local targetListItemData = targetListTable[targetListIndex]
            -- 战斗对象ID
            table[offset] = string.pack(">I8", targetListItemData)
            offset = offset + 1
        end
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function MapProtocol.decode(offset, protocol, data)
    if protocol == 20001 then
        local offset = offset
        -- 

        -- object
        local data = {}
        return data
    elseif protocol == 20011 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local fc = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 血量
            local hp = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 健康
            local health = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- object
            local attribute = {fc = fc, hp = hp, health = health}
            -- 技能列表
            local skill = {}
            local skillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for skillIndex = 1, skillLength do
                -- 
                -- 技能ID
                local skillId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 时间
                local time = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local number = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local battleSkill = {skillId = skillId, time = time, number = number}
                skill[skillIndex] = battleSkill
            end
            -- Buff列表
            local buff = {}
            local buffLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for buffIndex = 1, buffLength do
                -- 
                -- BuffID
                local buffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local overlap = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local battleBuff = {buffId = buffId, expireTime = expireTime, overlap = overlap}
                buff[buffIndex] = battleBuff
            end
            -- X坐标
            local x = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- Y坐标
            local y = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local fighter = {id = id, type = type, attribute = attribute, skill = skill, buff = buff, x = x, y = y}
            data[dataIndex] = fighter
        end
        return data
    elseif protocol == 20012 then
        local offset = offset
        -- 
        -- ID
        local id = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- X坐标
        local x = string.unpack(">I2", data, offset)
        offset = offset + 2
        -- Y坐标
        local y = string.unpack(">I2", data, offset)
        offset = offset + 2
        -- object
        local fighter = {id = id, x = x, y = y}
        return fighter
    elseif protocol == 20013 then
        local offset = offset
        -- 
        -- 战斗对象ID
        local id = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local fighter = {id = id}
        return fighter
    elseif protocol == 20014 then
        local offset = offset
        -- 
        -- 战斗对象Id
        local fighterId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 技能Id
        local performSkillId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 
        local fighterList = {}
        local fighterListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for fighterListIndex = 1, fighterListLength do
            -- 
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local fc = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 血量
            local hp = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 健康
            local health = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- object
            local attribute = {fc = fc, hp = hp, health = health}
            -- 技能列表
            local skill = {}
            local skillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for skillIndex = 1, skillLength do
                -- 
                -- 技能ID
                local skillId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 时间
                local time = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local number = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local battleSkill = {skillId = skillId, time = time, number = number}
                skill[skillIndex] = battleSkill
            end
            -- Buff列表
            local buff = {}
            local buffLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for buffIndex = 1, buffLength do
                -- 
                -- BuffID
                local buffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local overlap = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local battleBuff = {buffId = buffId, expireTime = expireTime, overlap = overlap}
                buff[buffIndex] = battleBuff
            end
            -- X坐标
            local x = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- Y坐标
            local y = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local fighter = {id = id, type = type, attribute = attribute, skill = skill, buff = buff, x = x, y = y}
            fighterList[fighterListIndex] = fighter
        end
        -- object
        local data = {fighterId = fighterId, performSkillId = performSkillId, fighterList = fighterList}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end