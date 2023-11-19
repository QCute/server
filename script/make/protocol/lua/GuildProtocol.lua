--- @class GuildQueryGuildRequest
--- @field protocol number 30101
--- @field data {
--- }

--- @class GuildQueryGuildRequest
--- @field protocol number 30101
--- @field data {
---     guildId: integer,                                                                           -- 公会ID
---     guildName: string,                                                                          -- 公会名字
---     createTime: integer,                                                                        -- 创建时间
---     leaderRoleId: integer,                                                                      -- 会长角色ID
---     leaderName: string,                                                                         -- 会长名字
--- }[]

--- @class GuildQueryRoleRequest
--- @field protocol number 30102
--- @field data {
--- }

--- @class GuildQueryRoleRequest
--- @field protocol number 30102
--- @field data {
---     roleId: integer,                                                                            -- 成员ID
---     job: integer,                                                                               -- 职位
---     joinTime: integer,                                                                          -- 加入时间
---     roleName: string,                                                                           -- 成员名字
---     sex: integer,                                                                               -- 性别
---     classes: integer,                                                                           -- 职业
---     vipLevel: integer,                                                                          -- Vip等级
--- }[]

--- @class GuildQueryApplyRequest
--- @field protocol number 30103
--- @field data {
--- }

--- @class GuildQueryApplyRequest
--- @field protocol number 30103
--- @field data {
---     roleId: integer,                                                                            -- 申请ID
---     applyTime: integer,                                                                         -- 申请时间
---     roleName: string,                                                                           -- 申请名字
---     sex: integer,                                                                               -- 性别
---     classes: integer,                                                                           -- 职业
---     vipLevel: integer,                                                                          -- Vip等级
--- }[]

--- @class GuildQuerySelfGuildRequest
--- @field protocol number 30104
--- @field data {
--- }

--- @class GuildQuerySelfGuildRequest
--- @field protocol number 30104
--- @field data {
---     guildId: integer,                                                                           -- 公会ID
---     guildName: string,                                                                          -- 公会名字
---     exp: integer,                                                                               -- 经验
---     wealth: integer,                                                                            -- 财富
---     level: integer,                                                                             -- 等级
---     createTime: integer,                                                                        -- 创建时间
---     notice: string,                                                                             -- 公告
---     leaderRoleId: integer,                                                                      -- 会长角色ID
---     leaderName: string,                                                                         -- 会长名字
--- }

--- @class GuildQuerySelfRoleRequest
--- @field protocol number 30105
--- @field data {
--- }

--- @class GuildQuerySelfRoleRequest
--- @field protocol number 30105
--- @field data {
---     roleId: integer,                                                                            -- 成员ID
---     job: integer,                                                                               -- 职位
---     joinTime: integer,                                                                          -- 加入时间
---     roleName: string,                                                                           -- 成员名字
---     sex: integer,                                                                               -- 性别
---     classes: integer,                                                                           -- 职业
---     vipLevel: integer,                                                                          -- Vip等级
--- }

--- @class GuildQuerySelfApplyRequest
--- @field protocol number 30106
--- @field data {
--- }

--- @class GuildQuerySelfApplyRequest
--- @field protocol number 30106
--- @field data {
---     guildId: integer,                                                                           -- 公会ID
---     applyTime: integer,                                                                         -- 申请时间
---     guildName: string,                                                                          -- 公会名字
--- }[]

--- @class GuildCreateRequest
--- @field protocol number 30107
--- @field data {
---     type: integer,                                                                              -- 类型
---     guildName: string,                                                                          -- 公会名
--- }

--- @class GuildCreateRequest
--- @field protocol number 30107
--- @field data string

--- @class GuildApplyRequest
--- @field protocol number 30108
--- @field data integer

--- @class GuildApplyRequest
--- @field protocol number 30108
--- @field data string

--- @class GuildCancelApplyRequest
--- @field protocol number 30109
--- @field data integer

--- @class GuildCancelApplyRequest
--- @field protocol number 30109
--- @field data string

--- @class GuildCancelAllApplyRequest
--- @field protocol number 30110
--- @field data {
--- }

--- @class GuildCancelAllApplyRequest
--- @field protocol number 30110
--- @field data string

--- @class GuildApproveApplyRequest
--- @field protocol number 30111
--- @field data integer

--- @class GuildApproveApplyRequest
--- @field protocol number 30111
--- @field data string

--- @class GuildApproveAllApplyRequest
--- @field protocol number 30112
--- @field data {
--- }

--- @class GuildApproveAllApplyRequest
--- @field protocol number 30112
--- @field data string

--- @class GuildRejectApplyRequest
--- @field protocol number 30113
--- @field data integer

--- @class GuildRejectApplyRequest
--- @field protocol number 30113
--- @field data string

--- @class GuildRejectAllApplyRequest
--- @field protocol number 30114
--- @field data {
--- }

--- @class GuildRejectAllApplyRequest
--- @field protocol number 30114
--- @field data string

--- @class GuildLeaveRequest
--- @field protocol number 30115
--- @field data {
--- }

--- @class GuildLeaveRequest
--- @field protocol number 30115
--- @field data string

--- @class GuildDismissRequest
--- @field protocol number 30116
--- @field data {
--- }

--- @class GuildDismissRequest
--- @field protocol number 30116
--- @field data string

--- @class GuildKickRequest
--- @field protocol number 30117
--- @field data integer

--- @class GuildKickRequest
--- @field protocol number 30117
--- @field data string

--- @class GuildUpdateJobRequest
--- @field protocol number 30118
--- @field data {
---     roleId: integer,                                                                            -- 角色ID
---     job: integer,                                                                               -- 位置
--- }

--- @class GuildUpdateJobRequest
--- @field protocol number 30118
--- @field data string

--- @class GuildUpgradeLevelRequest
--- @field protocol number 30119
--- @field data {
--- }

--- @class GuildUpgradeLevelRequest
--- @field protocol number 30119
--- @field data string

--- @class GuildChangeNoticeRequest
--- @field protocol number 30120
--- @field data string

--- @class GuildChangeNoticeRequest
--- @field protocol number 30120
--- @field data string

GuildProtocol = {}

function GuildProtocol.encode(offset, protocol, data)
    if protocol == 30101 then
        local table = {}

        return table
    elseif protocol == 30102 then
        local table = {}

        return table
    elseif protocol == 30103 then
        local table = {}

        return table
    elseif protocol == 30104 then
        local table = {}

        return table
    elseif protocol == 30105 then
        local table = {}

        return table
    elseif protocol == 30106 then
        local table = {}

        return table
    elseif protocol == 30107 then
        local table = {}

        -- 类型
        table[offset] = string.pack(">I1", data.type)
        offset = offset + 1
        -- 公会名
        table[offset] = string.pack(">s2", data.guildName)
        offset = offset + 1
        return table
    elseif protocol == 30108 then
        local table = {}
        -- 公会ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 30109 then
        local table = {}
        -- 公会ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 30110 then
        local table = {}

        return table
    elseif protocol == 30111 then
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 30112 then
        local table = {}

        return table
    elseif protocol == 30113 then
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 30114 then
        local table = {}

        return table
    elseif protocol == 30115 then
        local table = {}

        return table
    elseif protocol == 30116 then
        local table = {}

        return table
    elseif protocol == 30117 then
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 30118 then
        local table = {}

        -- 角色ID
        table[offset] = string.pack(">I8", data.roleId)
        offset = offset + 1
        -- 位置
        table[offset] = string.pack(">I1", data.job)
        offset = offset + 1
        return table
    elseif protocol == 30119 then
        local table = {}

        return table
    elseif protocol == 30120 then
        local table = {}
        -- 公告
        table[offset] = string.pack(">s2", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function GuildProtocol.decode(offset, protocol, bytes)
    if protocol == 30101 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 公会ID
            local dataDataGuildId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 公会名字
            local dataDataGuildName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataGuildName)
            -- 创建时间
            local dataDataCreateTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 会长角色ID
            local dataDataLeaderRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 会长名字
            local dataDataLeaderName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataLeaderName)
            -- object
            local dataData = {guildId = dataDataGuildId, guildName = dataDataGuildName, createTime = dataDataCreateTime, leaderRoleId = dataDataLeaderRoleId, leaderName = dataDataLeaderName}
            data[dataIndex] = dataData
        end
        return {protocol = 30101, data = data}
    elseif protocol == 30102 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 成员ID
            local dataDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 职位
            local dataDataJob = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 加入时间
            local dataDataJoinTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 成员名字
            local dataDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataRoleName)
            -- 性别
            local dataDataSex = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 职业
            local dataDataClasses = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- Vip等级
            local dataDataVipLevel = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataData = {roleId = dataDataRoleId, job = dataDataJob, joinTime = dataDataJoinTime, roleName = dataDataRoleName, sex = dataDataSex, classes = dataDataClasses, vipLevel = dataDataVipLevel}
            data[dataIndex] = dataData
        end
        return {protocol = 30102, data = data}
    elseif protocol == 30103 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 申请ID
            local dataDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 申请时间
            local dataDataApplyTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 申请名字
            local dataDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataRoleName)
            -- 性别
            local dataDataSex = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 职业
            local dataDataClasses = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- Vip等级
            local dataDataVipLevel = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- object
            local dataData = {roleId = dataDataRoleId, applyTime = dataDataApplyTime, roleName = dataDataRoleName, sex = dataDataSex, classes = dataDataClasses, vipLevel = dataDataVipLevel}
            data[dataIndex] = dataData
        end
        return {protocol = 30103, data = data}
    elseif protocol == 30104 then
        -- 
        -- 公会ID
        local dataGuildId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 公会名字
        local dataGuildName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataGuildName)
        -- 经验
        local dataExp = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 财富
        local dataWealth = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 等级
        local dataLevel = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 创建时间
        local dataCreateTime = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 公告
        local dataNotice = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataNotice)
        -- 会长角色ID
        local dataLeaderRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 会长名字
        local dataLeaderName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataLeaderName)
        -- object
        local data = {guildId = dataGuildId, guildName = dataGuildName, exp = dataExp, wealth = dataWealth, level = dataLevel, createTime = dataCreateTime, notice = dataNotice, leaderRoleId = dataLeaderRoleId, leaderName = dataLeaderName}
        return {protocol = 30104, data = data}
    elseif protocol == 30105 then
        -- 
        -- 成员ID
        local dataRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 职位
        local dataJob = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 加入时间
        local dataJoinTime = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 成员名字
        local dataRoleName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataRoleName)
        -- 性别
        local dataSex = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 职业
        local dataClasses = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- Vip等级
        local dataVipLevel = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- object
        local data = {roleId = dataRoleId, job = dataJob, joinTime = dataJoinTime, roleName = dataRoleName, sex = dataSex, classes = dataClasses, vipLevel = dataVipLevel}
        return {protocol = 30105, data = data}
    elseif protocol == 30106 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 公会ID
            local dataDataGuildId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 申请时间
            local dataDataApplyTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 公会名字
            local dataDataGuildName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataGuildName)
            -- object
            local dataData = {guildId = dataDataGuildId, applyTime = dataDataApplyTime, guildName = dataDataGuildName}
            data[dataIndex] = dataData
        end
        return {protocol = 30106, data = data}
    elseif protocol == 30107 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30107, data = data}
    elseif protocol == 30108 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30108, data = data}
    elseif protocol == 30109 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30109, data = data}
    elseif protocol == 30110 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30110, data = data}
    elseif protocol == 30111 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30111, data = data}
    elseif protocol == 30112 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30112, data = data}
    elseif protocol == 30113 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30113, data = data}
    elseif protocol == 30114 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30114, data = data}
    elseif protocol == 30115 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30115, data = data}
    elseif protocol == 30116 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30116, data = data}
    elseif protocol == 30117 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30117, data = data}
    elseif protocol == 30118 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30118, data = data}
    elseif protocol == 30119 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30119, data = data}
    elseif protocol == 30120 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 30120, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end