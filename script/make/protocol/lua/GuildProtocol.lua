function encodeGuildProtocol(offset, protocol, data)
    if protocol == 30101 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30102 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30103 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30104 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30105 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30106 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30107 then
        local offset = offset
        local table = {}
        -- 类型
        table[offset] = string.pack(">I1", data["type"])
        offset = offset + 1
        -- 公会名
        table[offset] = string.pack(">s2", data["guildName"])
        offset = offset + 1
        return table
    elseif protocol == 30108 then
        local offset = offset
        local table = {}
        -- 公会ID
        table[offset] = string.pack(">I8", data["guildId"])
        offset = offset + 1
        return table
    elseif protocol == 30109 then
        local offset = offset
        local table = {}
        -- 公会ID
        table[offset] = string.pack(">I8", data["guildId"])
        offset = offset + 1
        return table
    elseif protocol == 30110 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30111 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["roleId"])
        offset = offset + 1
        return table
    elseif protocol == 30112 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30113 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["roleId"])
        offset = offset + 1
        return table
    elseif protocol == 30114 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30115 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30116 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30117 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["roleId"])
        offset = offset + 1
        return table
    elseif protocol == 30118 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["roleId"])
        offset = offset + 1
        -- 位置
        table[offset] = string.pack(">I1", data["job"])
        offset = offset + 1
        return table
    elseif protocol == 30119 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 30120 then
        local offset = offset
        local table = {}
        -- 公告
        table[offset] = string.pack(">s2", data["notice"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeGuildProtocol(offset, protocol, data)
    if protocol == 30101 then
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
    elseif protocol == 30102 then
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
    elseif protocol == 30103 then
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
    elseif protocol == 30104 then
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
    elseif protocol == 30105 then
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
    elseif protocol == 30106 then
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
    elseif protocol == 30107 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30108 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30109 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30110 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30111 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30112 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30113 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30114 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30115 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30116 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30117 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30118 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30119 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 30120 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end