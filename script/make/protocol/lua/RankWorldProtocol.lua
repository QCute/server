RankWorldProtocol = {}

function RankWorldProtocol.encode(offset, protocol, data)
    if protocol == 19201 then
        local table = {}

        return table
    elseif protocol == 19202 then
        local table = {}

        return table
    elseif protocol == 19203 then
        local table = {}

        return table
    elseif protocol == 19204 then
        local table = {}

        return table
    elseif protocol == 19205 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function RankWorldProtocol.decode(offset, protocol, bytes)
    if protocol == 19201 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 类型
            local dataDataType = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 排名
            local dataDataOrder = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 键
            local dataDataKey = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 值
            local dataDataValue = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 时间
            local dataDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 名字
            local dataDataName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataName)
            -- 服务器ID
            local dataDataServerId = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {type = dataDataType, order = dataDataOrder, key = dataDataKey, value = dataDataValue, time = dataDataTime, name = dataDataName, serverId = dataDataServerId}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 19202 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 类型
            local dataDataType = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 排名
            local dataDataOrder = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 键
            local dataDataKey = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 值
            local dataDataValue = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 时间
            local dataDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 名字
            local dataDataName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataName)
            -- 服务器ID
            local dataDataServerId = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 
            -- 等级
            local dataDataOtherLevel = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 职业
            local dataDataOtherClasses = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataDataOther = {level = dataDataOtherLevel, classes = dataDataOtherClasses}
            -- object
            local dataData = {type = dataDataType, order = dataDataOrder, key = dataDataKey, value = dataDataValue, time = dataDataTime, name = dataDataName, serverId = dataDataServerId, other = dataDataOther}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 19203 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 类型
            local dataDataType = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 排名
            local dataDataOrder = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 键
            local dataDataKey = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 值
            local dataDataValue = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 时间
            local dataDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 名字
            local dataDataName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataName)
            -- 服务器ID
            local dataDataServerId = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 
            -- 等级
            local dataDataOtherLevel = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 职业
            local dataDataOtherClasses = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 性别
            local dataDataOtherSex = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataDataOther = {level = dataDataOtherLevel, classes = dataDataOtherClasses, sex = dataDataOtherSex}
            -- object
            local dataData = {type = dataDataType, order = dataDataOrder, key = dataDataKey, value = dataDataValue, time = dataDataTime, name = dataDataName, serverId = dataDataServerId, other = dataDataOther}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 19204 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 类型
            local dataDataType = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 排名
            local dataDataOrder = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 键
            local dataDataKey = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 值
            local dataDataValue = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 时间
            local dataDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 名字
            local dataDataName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataName)
            -- 服务器ID
            local dataDataServerId = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 
            -- 等级
            local dataDataOtherLevel = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 职业
            local dataDataOtherClasses = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 性别
            local dataDataOtherSex = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- VIP等级
            local dataDataOtherVipLevel = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataDataOther = {level = dataDataOtherLevel, classes = dataDataOtherClasses, sex = dataDataOtherSex, vipLevel = dataDataOtherVipLevel}
            -- object
            local dataData = {type = dataDataType, order = dataDataOrder, key = dataDataKey, value = dataDataValue, time = dataDataTime, name = dataDataName, serverId = dataDataServerId, other = dataDataOther}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 19205 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 类型
            local dataDataType = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 排名
            local dataDataOrder = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 键
            local dataDataKey = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 值
            local dataDataValue = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 时间
            local dataDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 名字
            local dataDataName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataName)
            -- 服务器ID
            local dataDataServerId = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 
            -- 等级
            local dataDataOtherLevel = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 职业
            local dataDataOtherClasses = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 性别
            local dataDataOtherSex = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- VIP等级
            local dataDataOtherVipLevel = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 头像
            local dataDataOtherAvatar = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataDataOther = {level = dataDataOtherLevel, classes = dataDataOtherClasses, sex = dataDataOtherSex, vipLevel = dataDataOtherVipLevel, avatar = dataDataOtherAvatar}
            -- object
            local dataData = {type = dataDataType, order = dataDataOrder, key = dataDataKey, value = dataDataValue, time = dataDataTime, name = dataDataName, serverId = dataDataServerId, other = dataDataOther}
            data[dataIndex] = dataData
        end
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end