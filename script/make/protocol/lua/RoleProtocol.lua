--- @class RoleQueryRequest
--- @field protocol number 10101
--- @field data {
--- }

--- @class RoleQueryRequest
--- @field protocol number 10101
--- @field data {
---     roleId: integer,                                                                            -- 角色ID
---     roleName: string,                                                                           -- 角色名
---     sex: integer,                                                                               -- 性别
---     classes: integer,                                                                           -- 职业
---     level: integer,                                                                             -- 等级
--- }

--- @class RoleAssetQueryRequest
--- @field protocol number 10102
--- @field data {
--- }

--- @class RoleAssetQueryRequest
--- @field protocol number 10102
--- @field data {
---     gold: integer,                                                                              -- 金币
---     silver: integer,                                                                            -- 银币
---     copper: integer,                                                                            -- 铜币
---     exp: integer,                                                                               -- 经验
--- }

--- @class RoleVipQueryRequest
--- @field protocol number 10103
--- @field data {
--- }

--- @class RoleVipQueryRequest
--- @field protocol number 10103
--- @field data {
---     vipLevel: integer,                                                                          -- 等级
---     exp: integer,                                                                               -- 经验
---     expireTime: integer,                                                                        -- 过期时间
--- }

RoleProtocol = {}

function RoleProtocol.encode(offset, protocol, data)
    if protocol == 10101 then
        local table = {}

        return table
    elseif protocol == 10102 then
        local table = {}

        return table
    elseif protocol == 10103 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function RoleProtocol.decode(offset, protocol, bytes)
    if protocol == 10101 then
        -- 
        -- 角色ID
        local dataRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 角色名
        local dataRoleName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataRoleName)
        -- 性别
        local dataSex = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 职业
        local dataClasses = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 等级
        local dataLevel = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {roleId = dataRoleId, roleName = dataRoleName, sex = dataSex, classes = dataClasses, level = dataLevel}
        return {protocol = 10101, data = data}
    elseif protocol == 10102 then
        -- 
        -- 金币
        local dataGold = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 银币
        local dataSilver = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 铜币
        local dataCopper = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 经验
        local dataExp = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {gold = dataGold, silver = dataSilver, copper = dataCopper, exp = dataExp}
        return {protocol = 10102, data = data}
    elseif protocol == 10103 then
        -- 
        -- 等级
        local dataVipLevel = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 经验
        local dataExp = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 过期时间
        local dataExpireTime = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- object
        local data = {vipLevel = dataVipLevel, exp = dataExp, expireTime = dataExpireTime}
        return {protocol = 10103, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end