function encodeRankProtocol(offset, protocol, data)
    if protocol == 19001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19002 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19003 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19004 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19005 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeRankProtocol(offset, protocol, data)
    if protocol == 19001 then
        local offset = offset
        -- 排行榜
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 类型
            local type = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 排名
            local order = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 键
            local key = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 值
            local value = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 时间
            local time = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 名字
            local name = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(name)
            -- 服务器ID
            local serverId = string.unpack(">I2", data, offset)
            offset = offset + 2
            list[listIndex] = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId}
        end
        return {list = list}
    elseif protocol == 19002 then
        local offset = offset
        -- 排行榜
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 类型
            local type = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 排名
            local order = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 键
            local key = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 值
            local value = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 时间
            local time = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 名字
            local name = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(name)
            -- 服务器ID
            local serverId = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 等级
            local level = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local classes = string.unpack(">I1", data, offset)
            offset = offset + 1
            list[listIndex] = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, level = level, classes = classes}
        end
        return {list = list}
    elseif protocol == 19003 then
        local offset = offset
        -- 排行榜
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 类型
            local type = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 排名
            local order = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 键
            local key = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 值
            local value = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 时间
            local time = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 名字
            local name = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(name)
            -- 服务器ID
            local serverId = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 等级
            local level = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local classes = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 性别
            local sex = string.unpack(">I1", data, offset)
            offset = offset + 1
            list[listIndex] = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, level = level, classes = classes, sex = sex}
        end
        return {list = list}
    elseif protocol == 19004 then
        local offset = offset
        -- 排行榜
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 类型
            local type = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 排名
            local order = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 键
            local key = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 值
            local value = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 时间
            local time = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 名字
            local name = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(name)
            -- 服务器ID
            local serverId = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 等级
            local level = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local classes = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 性别
            local sex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- VIP等级
            local vipLevel = string.unpack(">I1", data, offset)
            offset = offset + 1
            list[listIndex] = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, level = level, classes = classes, sex = sex, vipLevel = vipLevel}
        end
        return {list = list}
    elseif protocol == 19005 then
        local offset = offset
        -- 排行榜
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 类型
            local type = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 排名
            local order = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 键
            local key = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 值
            local value = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 时间
            local time = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 名字
            local name = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(name)
            -- 服务器ID
            local serverId = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 等级
            local level = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local classes = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 性别
            local sex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- VIP等级
            local vipLevel = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 头像
            local avatar = string.unpack(">I1", data, offset)
            offset = offset + 1
            list[listIndex] = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, level = level, classes = classes, sex = sex, vipLevel = vipLevel, avatar = avatar}
        end
        return {list = list}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end