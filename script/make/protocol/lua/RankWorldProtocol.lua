RankWorldProtocol = {}

function RankWorldProtocol.encode(offset, protocol, data)
    if protocol == 19201 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19202 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19203 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19204 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 19205 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function RankWorldProtocol.decode(offset, protocol, data)
    if protocol == 19201 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
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
            -- object
            local rank = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId}
            data[dataIndex] = rank
        end
        return data
    elseif protocol == 19202 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
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
            -- 
            -- 等级
            local otherLevel = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local otherClasses = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local other = {level = otherLevel, classes = otherClasses}
            -- object
            local rank = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, other = other}
            data[dataIndex] = rank
        end
        return data
    elseif protocol == 19203 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
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
            -- 
            -- 等级
            local otherLevel = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local otherClasses = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 性别
            local otherSex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local other = {level = otherLevel, classes = otherClasses, sex = otherSex}
            -- object
            local rank = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, other = other}
            data[dataIndex] = rank
        end
        return data
    elseif protocol == 19204 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
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
            -- 
            -- 等级
            local otherLevel = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local otherClasses = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 性别
            local otherSex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- VIP等级
            local otherVipLevel = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local other = {level = otherLevel, classes = otherClasses, sex = otherSex, vipLevel = otherVipLevel}
            -- object
            local rank = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, other = other}
            data[dataIndex] = rank
        end
        return data
    elseif protocol == 19205 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
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
            -- 
            -- 等级
            local otherLevel = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 职业
            local otherClasses = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 性别
            local otherSex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- VIP等级
            local otherVipLevel = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 头像
            local otherAvatar = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- object
            local other = {level = otherLevel, classes = otherClasses, sex = otherSex, vipLevel = otherVipLevel, avatar = otherAvatar}
            -- object
            local rank = {type = type, order = order, key = key, value = value, time = time, name = name, serverId = serverId, other = other}
            data[dataIndex] = rank
        end
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end