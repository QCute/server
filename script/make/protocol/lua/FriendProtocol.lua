FriendProtocol = {}

function FriendProtocol.encode(offset, protocol, data)
    if protocol == 11501 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11502 then
        local offset = offset
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11503 then
        local offset = offset
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11504 then
        local offset = offset
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11505 then
        local offset = offset
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11506 then
        local offset = offset
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function FriendProtocol.decode(offset, protocol, data)
    if protocol == 11501 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 好友角色ID
            local friendRoleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 好友名字
            local friendName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(friendName)
            -- 关系状态(申请:1/好友:2/黑名单:3)
            local relation = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 添加/修改状态时间
            local time = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local friend = {friendRoleId = friendRoleId, friendName = friendName, relation = relation, time = time}
            data[dataIndex] = friend
        end
        return data
    elseif protocol == 11502 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11503 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11504 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 好友角色ID
        local friendRoleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local data = {result = result, friendRoleId = friendRoleId}
        return data
    elseif protocol == 11505 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 好友角色ID
        local friendRoleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local data = {result = result, friendRoleId = friendRoleId}
        return data
    elseif protocol == 11506 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 好友角色ID
        local friendRoleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local data = {result = result, friendRoleId = friendRoleId}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end