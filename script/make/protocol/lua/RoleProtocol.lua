RoleProtocol = {}

function RoleProtocol.encode(offset, protocol, data)
    if protocol == 10101 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 10102 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 10103 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function RoleProtocol.decode(offset, protocol, data)
    if protocol == 10101 then
        local offset = offset
        -- Role
        -- 角色ID
        local roleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 角色名
        local roleName = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(roleName)
        -- 性别
        local sex = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 职业
        local classes = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 等级
        local level = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local role = {roleId = roleId, roleName = roleName, sex = sex, classes = classes, level = level}
        return {role = role}
    elseif protocol == 10102 then
        local offset = offset
        -- Asset
        -- 金币
        local gold = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 银币
        local silver = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- 铜币
        local copper = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 经验
        local exp = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local asset = {gold = gold, silver = silver, copper = copper, exp = exp}
        return {asset = asset}
    elseif protocol == 10103 then
        local offset = offset
        -- Vip
        -- 等级
        local vipLevel = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 经验
        local exp = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 过期时间
        local expireTime = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- object
        local vip = {vipLevel = vipLevel, exp = exp, expireTime = expireTime}
        return {vip = vip}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end