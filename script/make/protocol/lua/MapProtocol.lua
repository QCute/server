MapProtocol = {}

function MapProtocol.encode(offset, protocol, data)
    if protocol == 20001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 20011 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 20012 then
        local offset = offset
        local table = {}
        -- x坐标
        table[offset] = string.pack(">I2", data["x"])
        offset = offset + 1
        -- y坐标
        table[offset] = string.pack(">I2", data["y"])
        offset = offset + 1
        return table
    elseif protocol == 20014 then
        local offset = offset
        local table = {}
        -- 技能Id
        table[offset] = string.pack(">I4", data["skillId"])
        offset = offset + 1
        -- 对象列表
        local targetListTable = data["targetList"]
        table[offset] = string.pack(">I2", #targetListTable)
        offset = offset + 1
        for targetListIndex = 1, #targetListTable do
            local targetListItemData = targetListTable[targetListIndex]
            -- ID
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
        return {}
    elseif protocol == 20011 then
        local offset = offset
        -- 对象列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Fighter
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 技能列表
            local skill = {}
            local skillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for skillIndex = 1, skillLength do
                -- BattleSkill
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
                -- BattleBuff
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
            local fighter = {id = id, type = type, skill = skill, buff = buff, x = x, y = y}
            list[listIndex] = fighter
        end
        return {list = list}
    elseif protocol == 20012 then
        local offset = offset
        -- Fighter
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
        return {fighter = fighter}
    elseif protocol == 20013 then
        local offset = offset
        -- Fighter
        -- ID
        local id = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local fighter = {id = id}
        return {fighter = fighter}
    elseif protocol == 20014 then
        local offset = offset
        -- 战斗对象Id
        local fighterId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 技能Id
        local performSkillId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 对象列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Fighter
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 技能列表
            local skill = {}
            local skillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for skillIndex = 1, skillLength do
                -- BattleSkill
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
                -- BattleBuff
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
            local fighter = {id = id, type = type, skill = skill, buff = buff, x = x, y = y}
            list[listIndex] = fighter
        end
        return {fighterId = fighterId, performSkillId = performSkillId, list = list}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end