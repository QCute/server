--- @class AccountHeartbeatRequest
--- @field protocol number 10000
--- @field data {
--- }

--- @class AccountHeartbeatRequest
--- @field protocol number 10000
--- @field data string

--- @class AccountQueryRequest
--- @field protocol number 10001
--- @field data {
---     serverId: integer,                                                                          -- 服务器ID
---     accountName: string,                                                                        -- 账户名
--- }

--- @class AccountQueryRequest
--- @field protocol number 10001
--- @field data {
---     result: string,                                                                             -- 结果
---     list: {
---         roleId: integer,                                                                        -- 角色ID
---         roleName: string,                                                                       -- 角色名
---     }[],                                                                                        -- 角色名列表
--- }

--- @class AccountCreateRequest
--- @field protocol number 10002
--- @field data {
---     roleName: string,                                                                           -- 角色名
---     serverId: integer,                                                                          -- 服务器ID
---     accountName: string,                                                                        -- 账户名
---     sex: integer,                                                                               -- 性别
---     classes: integer,                                                                           -- 职业
---     channel: string,                                                                            -- 渠道
---     deviceId: string,                                                                           -- 设备
---     mac: string,                                                                                -- mac地址
---     deviceType: string,                                                                         -- 设备类型
--- }

--- @class AccountCreateRequest
--- @field protocol number 10002
--- @field data {
---     result: string,                                                                             -- 结果
---     roleId: integer,                                                                            -- 角色ID
---     roleName: string,                                                                           -- 角色名
--- }

--- @class AccountLoginRequest
--- @field protocol number 10003
--- @field data {
---     roleId: integer,                                                                            -- 角色ID
---     roleName: string,                                                                           -- 角色名
---     serverId: integer,                                                                          -- 服务器ID
---     accountName: string,                                                                        -- 账户名
--- }

--- @class AccountLoginRequest
--- @field protocol number 10003
--- @field data string

--- @class AccountLogoutRequest
--- @field protocol number 10004
--- @field data {
--- }

--- @class AccountLogoutRequest
--- @field protocol number 10004
--- @field data string

AccountProtocol = {}

function AccountProtocol.encode(offset, protocol, data)
    if protocol == 10000 then
        local table = {}

        return table
    elseif protocol == 10001 then
        local table = {}

        -- 服务器ID
        table[offset] = string.pack(">I2", data.serverId)
        offset = offset + 1
        -- 账户名
        table[offset] = string.pack(">s2", data.accountName)
        offset = offset + 1
        return table
    elseif protocol == 10002 then
        local table = {}

        -- 角色名
        table[offset] = string.pack(">s2", data.roleName)
        offset = offset + 1
        -- 服务器ID
        table[offset] = string.pack(">I2", data.serverId)
        offset = offset + 1
        -- 账户名
        table[offset] = string.pack(">s2", data.accountName)
        offset = offset + 1
        -- 性别
        table[offset] = string.pack(">I1", data.sex)
        offset = offset + 1
        -- 职业
        table[offset] = string.pack(">I1", data.classes)
        offset = offset + 1
        -- 渠道
        table[offset] = string.pack(">s2", data.channel)
        offset = offset + 1
        -- 设备
        table[offset] = string.pack(">s2", data.deviceId)
        offset = offset + 1
        -- mac地址
        table[offset] = string.pack(">s2", data.mac)
        offset = offset + 1
        -- 设备类型
        table[offset] = string.pack(">s2", data.deviceType)
        offset = offset + 1
        return table
    elseif protocol == 10003 then
        local table = {}

        -- 角色ID
        table[offset] = string.pack(">I8", data.roleId)
        offset = offset + 1
        -- 角色名
        table[offset] = string.pack(">s2", data.roleName)
        offset = offset + 1
        -- 服务器ID
        table[offset] = string.pack(">I2", data.serverId)
        offset = offset + 1
        -- 账户名
        table[offset] = string.pack(">s2", data.accountName)
        offset = offset + 1
        return table
    elseif protocol == 10004 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function AccountProtocol.decode(offset, protocol, bytes)
    if protocol == 10000 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 10000, data = data}
    elseif protocol == 10001 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 角色名列表
        local dataList = {}
        local dataListLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataListIndex = 1, dataListLength do
            -- 
            -- 角色ID
            local dataListDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色名
            local dataListDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataListDataRoleName)
            -- object
            local dataListData = {roleId = dataListDataRoleId, roleName = dataListDataRoleName}
            dataList[dataListIndex] = dataListData
        end
        -- object
        local data = {result = dataResult, list = dataList}
        return {protocol = 10001, data = data}
    elseif protocol == 10002 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 角色ID
        local dataRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 角色名
        local dataRoleName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataRoleName)
        -- object
        local data = {result = dataResult, roleId = dataRoleId, roleName = dataRoleName}
        return {protocol = 10002, data = data}
    elseif protocol == 10003 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 10003, data = data}
    elseif protocol == 10004 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 10004, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end