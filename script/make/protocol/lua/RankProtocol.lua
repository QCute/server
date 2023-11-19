--- @class RankLevelRequest
--- @field protocol number 19001
--- @field data {
--- }

--- @class RankLevelRequest
--- @field protocol number 19001
--- @field data {
---     type: integer,                                                                              -- 类型
---     order: integer,                                                                             -- 排名
---     key: integer,                                                                               -- 键
---     value: integer,                                                                             -- 值
---     time: integer,                                                                              -- 时间
---     name: string,                                                                               -- 名字
---     serverId: integer,                                                                          -- 服务器ID
--- }[]

--- @class RankFightRequest
--- @field protocol number 19002
--- @field data {
--- }

--- @class RankFightRequest
--- @field protocol number 19002
--- @field data {
---     type: integer,                                                                              -- 类型
---     order: integer,                                                                             -- 排名
---     key: integer,                                                                               -- 键
---     value: integer,                                                                             -- 值
---     time: integer,                                                                              -- 时间
---     name: string,                                                                               -- 名字
---     serverId: integer,                                                                          -- 服务器ID
---     other: {
---         level: integer,                                                                         -- 等级
---         classes: integer,                                                                       -- 职业
---     },                                                                                          -- 
--- }[]

--- @class RankAchievementRequest
--- @field protocol number 19003
--- @field data {
--- }

--- @class RankAchievementRequest
--- @field protocol number 19003
--- @field data {
---     type: integer,                                                                              -- 类型
---     order: integer,                                                                             -- 排名
---     key: integer,                                                                               -- 键
---     value: integer,                                                                             -- 值
---     time: integer,                                                                              -- 时间
---     name: string,                                                                               -- 名字
---     serverId: integer,                                                                          -- 服务器ID
---     other: {
---         level: integer,                                                                         -- 等级
---         classes: integer,                                                                       -- 职业
---         sex: integer,                                                                           -- 性别
---     },                                                                                          -- 
--- }[]

--- @class RankWealthRequest
--- @field protocol number 19004
--- @field data {
--- }

--- @class RankWealthRequest
--- @field protocol number 19004
--- @field data {
---     type: integer,                                                                              -- 类型
---     order: integer,                                                                             -- 排名
---     key: integer,                                                                               -- 键
---     value: integer,                                                                             -- 值
---     time: integer,                                                                              -- 时间
---     name: string,                                                                               -- 名字
---     serverId: integer,                                                                          -- 服务器ID
---     other: {
---         level: integer,                                                                         -- 等级
---         classes: integer,                                                                       -- 职业
---         sex: integer,                                                                           -- 性别
---         vipLevel: integer,                                                                      -- VIP等级
---     },                                                                                          -- 
--- }[]

--- @class RankClassesRequest
--- @field protocol number 19005
--- @field data {
--- }

--- @class RankClassesRequest
--- @field protocol number 19005
--- @field data {
---     type: integer,                                                                              -- 类型
---     order: integer,                                                                             -- 排名
---     key: integer,                                                                               -- 键
---     value: integer,                                                                             -- 值
---     time: integer,                                                                              -- 时间
---     name: string,                                                                               -- 名字
---     serverId: integer,                                                                          -- 服务器ID
---     other: {
---         level: integer,                                                                         -- 等级
---         classes: integer,                                                                       -- 职业
---         sex: integer,                                                                           -- 性别
---         vipLevel: integer,                                                                      -- VIP等级
---         avatar: integer,                                                                        -- 头像
---     },                                                                                          -- 
--- }[]

RankProtocol = {}

function RankProtocol.encode(offset, protocol, data)
    if protocol == 19001 then
        local table = {}

        return table
    elseif protocol == 19002 then
        local table = {}

        return table
    elseif protocol == 19003 then
        local table = {}

        return table
    elseif protocol == 19004 then
        local table = {}

        return table
    elseif protocol == 19005 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function RankProtocol.decode(offset, protocol, bytes)
    if protocol == 19001 then
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
        return {protocol = 19001, data = data}
    elseif protocol == 19002 then
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
        return {protocol = 19002, data = data}
    elseif protocol == 19003 then
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
        return {protocol = 19003, data = data}
    elseif protocol == 19004 then
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
        return {protocol = 19004, data = data}
    elseif protocol == 19005 then
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
        return {protocol = 19005, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end