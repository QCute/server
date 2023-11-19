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
        return data
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
        return data
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
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end