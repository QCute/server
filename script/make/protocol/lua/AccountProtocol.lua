AccountProtocol = {}

function AccountProtocol.encode(offset, protocol, data)
    if protocol == 10000 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 10001 then
        local offset = offset
        local table = {}
        -- 服务器ID
        table[offset] = string.pack(">I2", data["data"]["serverId"])
        offset = offset + 1
        -- 账户名
        table[offset] = string.pack(">s2", data["data"]["accountName"])
        offset = offset + 1
        return table
    elseif protocol == 10002 then
        local offset = offset
        local table = {}
        -- 角色名
        table[offset] = string.pack(">s2", data["data"]["roleName"])
        offset = offset + 1
        -- 服务器ID
        table[offset] = string.pack(">I2", data["data"]["serverId"])
        offset = offset + 1
        -- 账户名
        table[offset] = string.pack(">s2", data["data"]["accountName"])
        offset = offset + 1
        -- 性别
        table[offset] = string.pack(">I1", data["data"]["sex"])
        offset = offset + 1
        -- 职业
        table[offset] = string.pack(">I1", data["data"]["classes"])
        offset = offset + 1
        -- 渠道
        table[offset] = string.pack(">s2", data["data"]["channel"])
        offset = offset + 1
        -- 设备
        table[offset] = string.pack(">s2", data["data"]["deviceId"])
        offset = offset + 1
        -- mac地址
        table[offset] = string.pack(">s2", data["data"]["mac"])
        offset = offset + 1
        -- 设备类型
        table[offset] = string.pack(">s2", data["data"]["deviceType"])
        offset = offset + 1
        return table
    elseif protocol == 10003 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["data"]["roleId"])
        offset = offset + 1
        -- 角色名
        table[offset] = string.pack(">s2", data["data"]["roleName"])
        offset = offset + 1
        -- 服务器ID
        table[offset] = string.pack(">I2", data["data"]["serverId"])
        offset = offset + 1
        -- 账户名
        table[offset] = string.pack(">s2", data["data"]["accountName"])
        offset = offset + 1
        return table
    elseif protocol == 10004 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function AccountProtocol.decode(offset, protocol, data)
    if protocol == 10000 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 10001 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 角色名列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 
            -- 角色ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            -- object
            local listItem = {roleId = roleId, roleName = roleName}
            list[listIndex] = listItem
        end
        -- object
        local data = {result = result, list = list}
        return data
    elseif protocol == 10002 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 角色ID
        local roleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 角色名
        local roleName = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(roleName)
        -- object
        local data = {result = result, roleId = roleId, roleName = roleName}
        return data
    elseif protocol == 10003 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 10004 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end