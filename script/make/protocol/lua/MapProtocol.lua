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
        table[offset] = string.pack(">I2", data["x"])
        offset = offset + 1
        -- Y坐标
        table[offset] = string.pack(">I2", data["y"])
        offset = offset + 1
        return table
    elseif protocol == 20014 then
        local offset = offset
        local table = {}
        -- 技能Id
        table[offset] = string.pack(">I4", data["skillId"])
        offset = offset + 1
        -- 战斗对象ID列表
        local targetList = data["targetList"]
        table[offset] = string.pack(">I2", #targetList)
        offset = offset + 1
        for targetListIndex = 1, #targetList do
            local targetListItem = targetList[targetListIndex]
            -- 战斗对象ID
            table[offset] = string.pack(">I8", targetListItem)
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
        -- 地图编号
        local mapNo = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 地图ID
        local mapId = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 
        local fighter = {}
        local fighterLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for fighterIndex = 1, fighterLength do
            -- 
            -- ID
            local fighterId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local fighterType = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local fighterAttributeFc = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 血量
            local fighterAttributeHp = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 健康
            local fighterAttributeHealth = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- object
            local fighterAttribute = {fc = fighterAttributeFc, hp = fighterAttributeHp, health = fighterAttributeHealth}
            -- 技能列表
            local fighterSkill = {}
            local fighterSkillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for fighterSkillIndex = 1, fighterSkillLength do
                -- 
                -- 技能ID
                local fighterSkillSkillId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 时间
                local fighterSkillTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local fighterSkillNumber = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local fighterSkillBattleSkill = {skillId = fighterSkillSkillId, time = fighterSkillTime, number = fighterSkillNumber}
                fighterSkill[fighterSkillIndex] = fighterSkillBattleSkill
            end
            -- Buff列表
            local fighterBuff = {}
            local fighterBuffLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for fighterBuffIndex = 1, fighterBuffLength do
                -- 
                -- BuffID
                local fighterBuffBuffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local fighterBuffExpireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local fighterBuffOverlap = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local fighterBuffBattleBuff = {buffId = fighterBuffBuffId, expireTime = fighterBuffExpireTime, overlap = fighterBuffOverlap}
                fighterBuff[fighterBuffIndex] = fighterBuffBattleBuff
            end
            -- X坐标
            local fighterX = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- Y坐标
            local fighterY = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local fighterFighter = {id = fighterId, type = fighterType, attribute = fighterAttribute, skill = fighterSkill, buff = fighterBuff, x = fighterX, y = fighterY}
            fighter[fighterIndex] = fighterFighter
        end
        -- object
        local map = {mapNo = mapNo, mapId = mapId, fighter = fighter}
        return map
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
            local attributeFc = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 血量
            local attributeHp = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 健康
            local attributeHealth = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- object
            local attribute = {fc = attributeFc, hp = attributeHp, health = attributeHealth}
            -- 技能列表
            local skill = {}
            local skillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for skillIndex = 1, skillLength do
                -- 
                -- 技能ID
                local skillSkillId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 时间
                local skillTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local skillNumber = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local skillBattleSkill = {skillId = skillSkillId, time = skillTime, number = skillNumber}
                skill[skillIndex] = skillBattleSkill
            end
            -- Buff列表
            local buff = {}
            local buffLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for buffIndex = 1, buffLength do
                -- 
                -- BuffID
                local buffBuffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local buffExpireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local buffOverlap = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local buffBattleBuff = {buffId = buffBuffId, expireTime = buffExpireTime, overlap = buffOverlap}
                buff[buffIndex] = buffBattleBuff
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
            local fighterListId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local fighterListType = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local fighterListAttributeFc = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 血量
            local fighterListAttributeHp = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 健康
            local fighterListAttributeHealth = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- object
            local fighterListAttribute = {fc = fighterListAttributeFc, hp = fighterListAttributeHp, health = fighterListAttributeHealth}
            -- 技能列表
            local fighterListSkill = {}
            local fighterListSkillLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for fighterListSkillIndex = 1, fighterListSkillLength do
                -- 
                -- 技能ID
                local fighterListSkillSkillId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 时间
                local fighterListSkillTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local fighterListSkillNumber = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local fighterListSkillBattleSkill = {skillId = fighterListSkillSkillId, time = fighterListSkillTime, number = fighterListSkillNumber}
                fighterListSkill[fighterListSkillIndex] = fighterListSkillBattleSkill
            end
            -- Buff列表
            local fighterListBuff = {}
            local fighterListBuffLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for fighterListBuffIndex = 1, fighterListBuffLength do
                -- 
                -- BuffID
                local fighterListBuffBuffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local fighterListBuffExpireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local fighterListBuffOverlap = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- object
                local fighterListBuffBattleBuff = {buffId = fighterListBuffBuffId, expireTime = fighterListBuffExpireTime, overlap = fighterListBuffOverlap}
                fighterListBuff[fighterListBuffIndex] = fighterListBuffBattleBuff
            end
            -- X坐标
            local fighterListX = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- Y坐标
            local fighterListY = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local fighterListFighter = {id = fighterListId, type = fighterListType, attribute = fighterListAttribute, skill = fighterListSkill, buff = fighterListBuff, x = fighterListX, y = fighterListY}
            fighterList[fighterListIndex] = fighterListFighter
        end
        -- object
        local data = {fighterId = fighterId, performSkillId = performSkillId, fighterList = fighterList}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end