function encodeMapProtocol(offset, protocol, data)
    local switch = {
        [20012] = function()
            local offset = offset
            local table = {}
            -- x坐标
            table[offset] = string.pack(">I2", data["x"])
            offset = offset + 1
            -- y坐标
            table[offset] = string.pack(">I2", data["y"])
            offset = offset + 1
            return table
        end,
        [20014] = function()
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
                -- ID
                table[offset] = string.pack(">I8", targetListTable[targetListIndex]["targetId"])
                offset = offset + 1
            end
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

function decodeMapProtocol(offset, protocol, data)
    local switch = {
        [20011] = function()
            local offset = offset
            -- 对象列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- ID
                local id = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 类型
                local type = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 战力
                local fc = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 血量
                local hp = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 健康
                local health = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 技能列表
                local skill = {}
                local skillLength = string.unpack(">I2", data, offset)
                offset = offset + 2
                for skillIndex = 1, skillLength do
                    -- 技能ID
                    local skillId = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 时间
                    local time = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 数量
                    local number = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    skill[skillIndex] = {skillId = skillId, time = time, number = number}
                end
                -- Buff列表
                local buff = {}
                local buffLength = string.unpack(">I2", data, offset)
                offset = offset + 2
                for buffIndex = 1, buffLength do
                    -- BuffID
                    local buffId = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 过期时间
                    local expireTime = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 数量
                    local overlap = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    buff[buffIndex] = {buffId = buffId, expireTime = expireTime, overlap = overlap}
                end
                -- X坐标
                local x = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- Y坐标
                local y = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {id = id, type = type, fc = fc, hp = hp, health = health, skill = skill, buff = buff, x = x, y = y}
            end
            return {list = list}
        end,
        [20012] = function()
            local offset = offset
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- X坐标
            local x = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- Y坐标
            local y = string.unpack(">I2", data, offset)
            offset = offset + 2
            return {id = id, x = x, y = y}
        end,
        [20013] = function()
            local offset = offset
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            return {id = id}
        end,
        [20014] = function()
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
                -- ID
                local id = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 类型
                local type = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 战力
                local fc = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 血量
                local hp = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 健康
                local health = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 技能列表
                local skill = {}
                local skillLength = string.unpack(">I2", data, offset)
                offset = offset + 2
                for skillIndex = 1, skillLength do
                    -- 技能ID
                    local skillId = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 时间
                    local time = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 数量
                    local number = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    skill[skillIndex] = {skillId = skillId, time = time, number = number}
                end
                -- Buff列表
                local buff = {}
                local buffLength = string.unpack(">I2", data, offset)
                offset = offset + 2
                for buffIndex = 1, buffLength do
                    -- BuffID
                    local buffId = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 过期时间
                    local expireTime = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 数量
                    local overlap = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    buff[buffIndex] = {buffId = buffId, expireTime = expireTime, overlap = overlap}
                end
                -- X坐标
                local x = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- Y坐标
                local y = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {id = id, type = type, fc = fc, hp = hp, health = health, skill = skill, buff = buff, x = x, y = y}
            end
            return {fighterId = fighterId, performSkillId = performSkillId, list = list}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end