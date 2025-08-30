FriendProtocol = {}

function FriendProtocol.encode(offset, protocol, data)
    if protocol == 11501 then
        local table = {}

        return table
    elseif protocol == 11502 then
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11503 then
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11504 then
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11505 then
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11506 then
        local table = {}
        -- 好友角色ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function FriendProtocol.decode(offset, protocol, bytes)
    if protocol == 11501 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 好友角色ID
            local dataDataFriendRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 好友名字
            local dataDataFriendName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataFriendName)
            -- 关系状态(申请:1/好友:2/黑名单:3)
            local dataDataRelation = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 添加/修改状态时间
            local dataDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {friendRoleId = dataDataFriendRoleId, friendName = dataDataFriendName, relation = dataDataRelation, time = dataDataTime}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 11502 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11503 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11504 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 好友角色ID
        local dataFriendRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {result = dataResult, friendRoleId = dataFriendRoleId}
        return data
    elseif protocol == 11505 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 好友角色ID
        local dataFriendRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {result = dataResult, friendRoleId = dataFriendRoleId}
        return data
    elseif protocol == 11506 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 好友角色ID
        local dataFriendRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {result = dataResult, friendRoleId = dataFriendRoleId}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end