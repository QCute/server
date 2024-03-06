ChatProtocol = {}

function ChatProtocol.encode(offset, protocol, data)
    if protocol == 11602 then
        local offset = offset
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data)
        offset = offset + 1
        return table
    elseif protocol == 11603 then
        local offset = offset
        local table = {}
        -- 类型
        table[offset] = string.pack(">I1", data["type"])
        offset = offset + 1
        -- 消息
        table[offset] = string.pack(">s2", data["message"])
        offset = offset + 1
        return table
    elseif protocol == 11604 then
        local offset = offset
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data)
        offset = offset + 1
        return table
    elseif protocol == 11605 then
        local offset = offset
        local table = {}
        -- 类型
        table[offset] = string.pack(">I1", data["type"])
        offset = offset + 1
        -- 消息
        table[offset] = string.pack(">s2", data["message"])
        offset = offset + 1
        return table
    elseif protocol == 11606 then
        local offset = offset
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data)
        offset = offset + 1
        return table
    elseif protocol == 11607 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["roleId"])
        offset = offset + 1
        -- 类型
        table[offset] = string.pack(">I1", data["type"])
        offset = offset + 1
        -- 消息
        table[offset] = string.pack(">s2", data["message"])
        offset = offset + 1
        return table
    elseif protocol == 11608 then
        local offset = offset
        local table = {}
        -- 角色ID
        table[offset] = string.pack(">I8", data["roleId"])
        offset = offset + 1
        -- 页
        table[offset] = string.pack(">I2", data["page"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function ChatProtocol.decode(offset, protocol, data)
    if protocol == 11602 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名字
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 消息内容
            local message = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(message)
            -- object
            local systemChat = {id = id, roleId = roleId, roleName = roleName, type = type, message = message}
            data[dataIndex] = systemChat
        end
        return data
    elseif protocol == 11603 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 
        -- ID
        local worldChatId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 角色ID
        local worldChatRoleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 角色名字
        local worldChatRoleName = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(worldChatRoleName)
        -- 类型
        local worldChatType = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 消息内容
        local worldChatMessage = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(worldChatMessage)
        -- object
        local worldChat = {id = worldChatId, roleId = worldChatRoleId, roleName = worldChatRoleName, type = worldChatType, message = worldChatMessage}
        -- object
        local data = {result = result, worldChat = worldChat}
        return data
    elseif protocol == 11604 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名字
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 消息内容
            local message = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(message)
            -- object
            local worldChat = {id = id, roleId = roleId, roleName = roleName, type = type, message = message}
            data[dataIndex] = worldChat
        end
        return data
    elseif protocol == 11605 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 
        -- ID
        local guildChatId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 角色ID
        local guildChatRoleId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 角色名字
        local guildChatRoleName = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(guildChatRoleName)
        -- 类型
        local guildChatType = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 消息内容
        local guildChatMessage = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(guildChatMessage)
        -- object
        local guildChat = {id = guildChatId, roleId = guildChatRoleId, roleName = guildChatRoleName, type = guildChatType, message = guildChatMessage}
        -- object
        local data = {result = result, guildChat = guildChat}
        return data
    elseif protocol == 11606 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local id = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名字
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 消息内容
            local message = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(message)
            -- object
            local guildChat = {id = id, roleId = roleId, roleName = roleName, type = type, message = message}
            data[dataIndex] = guildChat
        end
        return data
    elseif protocol == 11607 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 
        -- 发送者角色ID
        local privateChatSenderId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 接收者角色ID
        local privateChatReceiverId = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- 类型
        local privateChatType = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 消息内容
        local privateChatMessage = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(privateChatMessage)
        -- object
        local privateChat = {senderId = privateChatSenderId, receiverId = privateChatReceiverId, type = privateChatType, message = privateChatMessage}
        -- object
        local data = {result = result, privateChat = privateChat}
        return data
    elseif protocol == 11608 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 发送者角色ID
            local senderId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 接收者角色ID
            local receiverId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 类型
            local type = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 消息内容
            local message = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(message)
            -- object
            local privateChat = {senderId = senderId, receiverId = receiverId, type = type, message = message}
            data[dataIndex] = privateChat
        end
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end