--- @class RankCenterLevelRequest
--- @field protocol number 19101
--- @field data {
--- }

--- @class RankCenterLevelRequest
--- @field protocol number 19101
--- @field data {
---     type: integer,                                                                              -- 类型
---     order: integer,                                                                             -- 排名
---     key: integer,                                                                               -- 键
---     value: integer,                                                                             -- 值
---     time: integer,                                                                              -- 时间
---     name: string,                                                                               -- 名字
---     serverId: integer,                                                                          -- 服务器ID
--- }[]

--- @class RankCenterFightRequest
--- @field protocol number 19102
--- @field data {
--- }

--- @class RankCenterFightRequest
--- @field protocol number 19102
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

--- @class RankCenterAchievementRequest
--- @field protocol number 19103
--- @field data {
--- }

--- @class RankCenterAchievementRequest
--- @field protocol number 19103
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

--- @class RankCenterWealthRequest
--- @field protocol number 19104
--- @field data {
--- }

--- @class RankCenterWealthRequest
--- @field protocol number 19104
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

--- @class RankCenterClassesRequest
--- @field protocol number 19105
--- @field data {
--- }

--- @class RankCenterClassesRequest
--- @field protocol number 19105
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

RankCenterProtocol = {}

function RankCenterProtocol.encode(offset, protocol, data)
    if protocol == 19101 then
        local table = {}

        return table
    elseif protocol == 19102 then
        local table = {}

        return table
    elseif protocol == 19103 then
        local table = {}

        return table
    elseif protocol == 19104 then
        local table = {}

        return table
    elseif protocol == 19105 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function RankCenterProtocol.decode(offset, protocol, bytes)
    if protocol == 19101 then
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
        return {protocol = 19101, data = data}
    elseif protocol == 19102 then
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
        return {protocol = 19102, data = data}
    elseif protocol == 19103 then
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
        return {protocol = 19103, data = data}
    elseif protocol == 19104 then
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
        return {protocol = 19104, data = data}
    elseif protocol == 19105 then
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
        return {protocol = 19105, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end