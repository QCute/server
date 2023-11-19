--- @class MapQueryRequest
--- @field protocol number 20001
--- @field data {
--- }

--- @class MapQueryRequest
--- @field protocol number 20001
--- @field data {
---     mapNo: integer,                                                                             -- 地图编号
---     mapId: integer,                                                                             -- 地图ID
---     fighter: {
---         id: integer,                                                                            -- ID
---         type: integer,                                                                          -- 类型
---         attribute: {
---             fc: integer,                                                                        -- 战力
---             hp: integer,                                                                        -- 血量
---             health: integer,                                                                    -- 健康
---         },                                                                                      -- 属性
---         skill: {
---             skillId: integer,                                                                   -- 技能ID
---             time: integer,                                                                      -- 时间
---             number: integer,                                                                    -- 数量
---         }[],                                                                                    -- 技能列表
---         buff: {
---             buffId: integer,                                                                    -- BuffID
---             expireTime: integer,                                                                -- 过期时间
---             overlap: integer,                                                                   -- 数量
---         }[],                                                                                    -- Buff列表
---         x: integer,                                                                             -- X坐标
---         y: integer,                                                                             -- Y坐标
---     }[],                                                                                        -- 
--- }



--- @class MapFighterRequest
--- @field protocol number 20011
--- @field data {
---     id: integer,                                                                                -- ID
---     type: integer,                                                                              -- 类型
---     attribute: {
---         fc: integer,                                                                            -- 战力
---         hp: integer,                                                                            -- 血量
---         health: integer,                                                                        -- 健康
---     },                                                                                          -- 属性
---     skill: {
---         skillId: integer,                                                                       -- 技能ID
---         time: integer,                                                                          -- 时间
---         number: integer,                                                                        -- 数量
---     }[],                                                                                        -- 技能列表
---     buff: {
---         buffId: integer,                                                                        -- BuffID
---         expireTime: integer,                                                                    -- 过期时间
---         overlap: integer,                                                                       -- 数量
---     }[],                                                                                        -- Buff列表
---     x: integer,                                                                                 -- X坐标
---     y: integer,                                                                                 -- Y坐标
--- }[]

--- @class MapFighterMoveRequest
--- @field protocol number 20012
--- @field data {
---     x: integer,                                                                                 -- X坐标
---     y: integer,                                                                                 -- Y坐标
--- }

--- @class MapFighterMoveRequest
--- @field protocol number 20012
--- @field data {
---     id: integer,                                                                                -- ID
---     x: integer,                                                                                 -- X坐标
---     y: integer,                                                                                 -- Y坐标
--- }



--- @class MapFighterLeaveRequest
--- @field protocol number 20013
--- @field data {
---     id: integer,                                                                                -- 战斗对象ID
--- }

--- @class MapAttackRequest
--- @field protocol number 20014
--- @field data {
---     skillId: integer,                                                                           -- 技能Id
---     targetList: integer[],                                                                      -- 战斗对象ID列表
--- }

--- @class MapAttackRequest
--- @field protocol number 20014
--- @field data {
---     fighterId: integer,                                                                         -- 战斗对象Id
---     performSkillId: integer,                                                                    -- 技能Id
---     fighterList: {
---         id: integer,                                                                            -- ID
---         type: integer,                                                                          -- 类型
---         attribute: {
---             fc: integer,                                                                        -- 战力
---             hp: integer,                                                                        -- 血量
---             health: integer,                                                                    -- 健康
---         },                                                                                      -- 属性
---         skill: {
---             skillId: integer,                                                                   -- 技能ID
---             time: integer,                                                                      -- 时间
---             number: integer,                                                                    -- 数量
---         }[],                                                                                    -- 技能列表
---         buff: {
---             buffId: integer,                                                                    -- BuffID
---             expireTime: integer,                                                                -- 过期时间
---             overlap: integer,                                                                   -- 数量
---         }[],                                                                                    -- Buff列表
---         x: integer,                                                                             -- X坐标
---         y: integer,                                                                             -- Y坐标
---     }[],                                                                                        -- 
--- }

MapProtocol = {}

function MapProtocol.encode(offset, protocol, data)
    if protocol == 20001 then
        local table = {}

        return table
    elseif protocol == 20012 then
        local table = {}

        -- X坐标
        table[offset] = string.pack(">I2", data.x)
        offset = offset + 1
        -- Y坐标
        table[offset] = string.pack(">I2", data.y)
        offset = offset + 1
        return table
    elseif protocol == 20014 then
        local table = {}

        -- 技能Id
        table[offset] = string.pack(">I4", data.skillId)
        offset = offset + 1
        local dataTargetList = data.targetList;
        -- 战斗对象ID列表
        table[offset] = string.pack(">I2", #dataTargetList)
        offset = offset + 1
        for dataTargetListIndex = 1, #dataTargetList do
            local dataTargetListData = dataTargetList[dataTargetListIndex]
            -- 战斗对象ID
            table[offset] = string.pack(">I8", dataTargetListData)
            offset = offset + 1
        end
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function MapProtocol.decode(offset, protocol, bytes)
    if protocol == 20001 then
        -- 
        -- 地图编号
        local dataMapNo = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 地图ID
        local dataMapId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 
        local dataFighter = {}
        local dataFighterLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataFighterIndex = 1, dataFighterLength do
            -- 
            -- ID
            local dataFighterDataId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 类型
            local dataFighterDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local dataFighterDataAttributeFc = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 血量
            local dataFighterDataAttributeHp = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 健康
            local dataFighterDataAttributeHealth = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- object
            local dataFighterDataAttribute = {fc = dataFighterDataAttributeFc, hp = dataFighterDataAttributeHp, health = dataFighterDataAttributeHealth}
            -- 技能列表
            local dataFighterDataSkill = {}
            local dataFighterDataSkillLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataFighterDataSkillIndex = 1, dataFighterDataSkillLength do
                -- 
                -- 技能ID
                local dataFighterDataSkillDataSkillId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 时间
                local dataFighterDataSkillDataTime = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataFighterDataSkillDataNumber = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- object
                local dataFighterDataSkillData = {skillId = dataFighterDataSkillDataSkillId, time = dataFighterDataSkillDataTime, number = dataFighterDataSkillDataNumber}
                dataFighterDataSkill[dataFighterDataSkillIndex] = dataFighterDataSkillData
            end
            -- Buff列表
            local dataFighterDataBuff = {}
            local dataFighterDataBuffLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataFighterDataBuffIndex = 1, dataFighterDataBuffLength do
                -- 
                -- BuffID
                local dataFighterDataBuffDataBuffId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 过期时间
                local dataFighterDataBuffDataExpireTime = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataFighterDataBuffDataOverlap = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- object
                local dataFighterDataBuffData = {buffId = dataFighterDataBuffDataBuffId, expireTime = dataFighterDataBuffDataExpireTime, overlap = dataFighterDataBuffDataOverlap}
                dataFighterDataBuff[dataFighterDataBuffIndex] = dataFighterDataBuffData
            end
            -- X坐标
            local dataFighterDataX = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- Y坐标
            local dataFighterDataY = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataFighterData = {id = dataFighterDataId, type = dataFighterDataType, attribute = dataFighterDataAttribute, skill = dataFighterDataSkill, buff = dataFighterDataBuff, x = dataFighterDataX, y = dataFighterDataY}
            dataFighter[dataFighterIndex] = dataFighterData
        end
        -- object
        local data = {mapNo = dataMapNo, mapId = dataMapId, fighter = dataFighter}
        return {protocol = 20001, data = data}
    elseif protocol == 20011 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local dataDataId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local dataDataAttributeFc = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 血量
            local dataDataAttributeHp = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 健康
            local dataDataAttributeHealth = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- object
            local dataDataAttribute = {fc = dataDataAttributeFc, hp = dataDataAttributeHp, health = dataDataAttributeHealth}
            -- 技能列表
            local dataDataSkill = {}
            local dataDataSkillLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataDataSkillIndex = 1, dataDataSkillLength do
                -- 
                -- 技能ID
                local dataDataSkillDataSkillId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 时间
                local dataDataSkillDataTime = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataDataSkillDataNumber = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- object
                local dataDataSkillData = {skillId = dataDataSkillDataSkillId, time = dataDataSkillDataTime, number = dataDataSkillDataNumber}
                dataDataSkill[dataDataSkillIndex] = dataDataSkillData
            end
            -- Buff列表
            local dataDataBuff = {}
            local dataDataBuffLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataDataBuffIndex = 1, dataDataBuffLength do
                -- 
                -- BuffID
                local dataDataBuffDataBuffId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 过期时间
                local dataDataBuffDataExpireTime = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataDataBuffDataOverlap = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- object
                local dataDataBuffData = {buffId = dataDataBuffDataBuffId, expireTime = dataDataBuffDataExpireTime, overlap = dataDataBuffDataOverlap}
                dataDataBuff[dataDataBuffIndex] = dataDataBuffData
            end
            -- X坐标
            local dataDataX = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- Y坐标
            local dataDataY = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {id = dataDataId, type = dataDataType, attribute = dataDataAttribute, skill = dataDataSkill, buff = dataDataBuff, x = dataDataX, y = dataDataY}
            data[dataIndex] = dataData
        end
        return {protocol = 20011, data = data}
    elseif protocol == 20012 then
        -- 
        -- ID
        local dataId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- X坐标
        local dataX = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        -- Y坐标
        local dataY = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        -- object
        local data = {id = dataId, x = dataX, y = dataY}
        return {protocol = 20012, data = data}
    elseif protocol == 20013 then
        -- 
        -- 战斗对象ID
        local dataId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {id = dataId}
        return {protocol = 20013, data = data}
    elseif protocol == 20014 then
        -- 
        -- 战斗对象Id
        local dataFighterId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 技能Id
        local dataPerformSkillId = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 
        local dataFighterList = {}
        local dataFighterListLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataFighterListIndex = 1, dataFighterListLength do
            -- 
            -- ID
            local dataFighterListDataId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 类型
            local dataFighterListDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 属性
            -- 战力
            local dataFighterListDataAttributeFc = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 血量
            local dataFighterListDataAttributeHp = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 健康
            local dataFighterListDataAttributeHealth = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- object
            local dataFighterListDataAttribute = {fc = dataFighterListDataAttributeFc, hp = dataFighterListDataAttributeHp, health = dataFighterListDataAttributeHealth}
            -- 技能列表
            local dataFighterListDataSkill = {}
            local dataFighterListDataSkillLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataFighterListDataSkillIndex = 1, dataFighterListDataSkillLength do
                -- 
                -- 技能ID
                local dataFighterListDataSkillDataSkillId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 时间
                local dataFighterListDataSkillDataTime = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataFighterListDataSkillDataNumber = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- object
                local dataFighterListDataSkillData = {skillId = dataFighterListDataSkillDataSkillId, time = dataFighterListDataSkillDataTime, number = dataFighterListDataSkillDataNumber}
                dataFighterListDataSkill[dataFighterListDataSkillIndex] = dataFighterListDataSkillData
            end
            -- Buff列表
            local dataFighterListDataBuff = {}
            local dataFighterListDataBuffLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataFighterListDataBuffIndex = 1, dataFighterListDataBuffLength do
                -- 
                -- BuffID
                local dataFighterListDataBuffDataBuffId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 过期时间
                local dataFighterListDataBuffDataExpireTime = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataFighterListDataBuffDataOverlap = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- object
                local dataFighterListDataBuffData = {buffId = dataFighterListDataBuffDataBuffId, expireTime = dataFighterListDataBuffDataExpireTime, overlap = dataFighterListDataBuffDataOverlap}
                dataFighterListDataBuff[dataFighterListDataBuffIndex] = dataFighterListDataBuffData
            end
            -- X坐标
            local dataFighterListDataX = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- Y坐标
            local dataFighterListDataY = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataFighterListData = {id = dataFighterListDataId, type = dataFighterListDataType, attribute = dataFighterListDataAttribute, skill = dataFighterListDataSkill, buff = dataFighterListDataBuff, x = dataFighterListDataX, y = dataFighterListDataY}
            dataFighterList[dataFighterListIndex] = dataFighterListData
        end
        -- object
        local data = {fighterId = dataFighterId, performSkillId = dataPerformSkillId, fighterList = dataFighterList}
        return {protocol = 20014, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end