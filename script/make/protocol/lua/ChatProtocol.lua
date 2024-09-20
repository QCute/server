ChatProtocol = {}

function ChatProtocol.encode(offset, protocol, data)
    if protocol == 11602 then
        local offset = offset
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data["page"])
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
        table[offset] = string.pack(">I2", data["page"])
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
        table[offset] = string.pack(">I2", data["page"])
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
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- SystemChat
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
            list[listIndex] = systemChat
        end
        return {list = list}
    elseif protocol == 11603 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- WorldChat
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
        return {result = result, worldChat = worldChat}
    elseif protocol == 11604 then
        local offset = offset
        -- 
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- WorldChat
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
            list[listIndex] = worldChat
        end
        return {list = list}
    elseif protocol == 11605 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- GuildChat
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
        return {result = result, guildChat = guildChat}
    elseif protocol == 11606 then
        local offset = offset
        -- 
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- GuildChat
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
            list[listIndex] = guildChat
        end
        return {list = list}
    elseif protocol == 11607 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- PrivateChat
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
        return {result = result, privateChat = privateChat}
    elseif protocol == 11608 then
        local offset = offset
        -- 
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- PrivateChat
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
            list[listIndex] = privateChat
        end
        return {list = list}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end