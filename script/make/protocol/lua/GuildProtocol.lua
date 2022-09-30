function encodeGuildProtocol(offset, protocol, data)
    local switch = {
        [30107] = function()
            local offset = offset
            local table = {}
            -- 类型
            table[offset] = string.pack(">I1", data["type"])
            offset = offset + 1
            -- 公会名
            table[offset] = string.pack(">s2", data["guildName"])
            offset = offset + 1
            return table
        end,
        [30108] = function()
            local offset = offset
            local table = {}
            -- 公会ID
            table[offset] = string.pack(">I8", data["guildId"])
            offset = offset + 1
            return table
        end,
        [30109] = function()
            local offset = offset
            local table = {}
            -- 公会ID
            table[offset] = string.pack(">I8", data["guildId"])
            offset = offset + 1
            return table
        end,
        [30111] = function()
            local offset = offset
            local table = {}
            -- 角色ID
            table[offset] = string.pack(">I8", data["roleId"])
            offset = offset + 1
            return table
        end,
        [30113] = function()
            local offset = offset
            local table = {}
            -- 角色ID
            table[offset] = string.pack(">I8", data["roleId"])
            offset = offset + 1
            return table
        end,
        [30117] = function()
            local offset = offset
            local table = {}
            -- 角色ID
            table[offset] = string.pack(">I8", data["roleId"])
            offset = offset + 1
            return table
        end,
        [30118] = function()
            local offset = offset
            local table = {}
            -- 角色ID
            table[offset] = string.pack(">I8", data["roleId"])
            offset = offset + 1
            -- 位置
            table[offset] = string.pack(">I1", data["job"])
            offset = offset + 1
            return table
        end,
        [30120] = function()
            local offset = offset
            local table = {}
            -- 公告
            table[offset] = string.pack(">s2", data["notice"])
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

function decodeGuildProtocol(offset, protocol, data)
    local switch = {
        [30101] = function()
            local offset = offset
            -- 公会列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 公会ID
                local guildId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 公会名字
                local guildName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(guildName)
                -- 创建时间
                local createTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 会长角色ID
                local leaderRoleId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 会长名字
                local leaderName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(leaderName)
                list[listIndex] = {guildId = guildId, guildName = guildName, createTime = createTime, leaderRoleId = leaderRoleId, leaderName = leaderName}
            end
            return {list = list}
        end,
        [30102] = function()
            local offset = offset
            -- 成员列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 成员ID
                local roleId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 职位
                local job = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 加入时间
                local joinTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 成员名字
                local roleName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(roleName)
                -- 性别
                local sex = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 职业
                local classes = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- Vip等级
                local vipLevel = string.unpack(">I1", data, offset)
                offset = offset + 1
                list[listIndex] = {roleId = roleId, job = job, joinTime = joinTime, roleName = roleName, sex = sex, classes = classes, vipLevel = vipLevel}
            end
            return {list = list}
        end,
        [30103] = function()
            local offset = offset
            -- 申请列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 申请ID
                local roleId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 申请时间
                local applyTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 申请名字
                local roleName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(roleName)
                -- 性别
                local sex = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- 职业
                local classes = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- Vip等级
                local vipLevel = string.unpack(">I1", data, offset)
                offset = offset + 1
                list[listIndex] = {roleId = roleId, applyTime = applyTime, roleName = roleName, sex = sex, classes = classes, vipLevel = vipLevel}
            end
            return {list = list}
        end,
        [30104] = function()
            local offset = offset
            -- 公会ID
            local guildId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 公会名字
            local guildName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(guildName)
            -- 经验
            local exp = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 财富
            local wealth = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 等级
            local level = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 创建时间
            local createTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 公告
            local notice = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(notice)
            -- 会长角色ID
            local leaderRoleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 会长名字
            local leaderName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(leaderName)
            return {guildId = guildId, guildName = guildName, exp = exp, wealth = wealth, level = level, createTime = createTime, notice = notice, leaderRoleId = leaderRoleId, leaderName = leaderName}
        end,
        [30105] = function()
            local offset = offset
            -- 成员ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 职位
            local job = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 加入时间
            local joinTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 成员名字
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            -- 性别
            local sex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 职业
            local classes = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- Vip等级
            local vipLevel = string.unpack(">I1", data, offset)
            offset = offset + 1
            return {roleId = roleId, job = job, joinTime = joinTime, roleName = roleName, sex = sex, classes = classes, vipLevel = vipLevel}
        end,
        [30106] = function()
            local offset = offset
            -- 
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 公会ID
                local guildId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 申请时间
                local applyTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 公会名字
                local guildName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(guildName)
                list[listIndex] = {guildId = guildId, applyTime = applyTime, guildName = guildName}
            end
            return {list = list}
        end,
        [30107] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30108] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30109] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30110] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30111] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30112] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30113] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30114] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30115] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30116] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30117] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30118] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30119] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [30120] = function()
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