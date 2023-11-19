--- @class ChatGetSystemListRequest
--- @field protocol number 11602
--- @field data integer

--- @class ChatGetSystemListRequest
--- @field protocol number 11602
--- @field data {
---     id: integer,                                                                                -- ID
---     roleId: integer,                                                                            -- 角色ID
---     roleName: string,                                                                           -- 角色名字
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息内容
--- }[]

--- @class ChatWorldRequest
--- @field protocol number 11603
--- @field data {
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息
--- }

--- @class ChatWorldRequest
--- @field protocol number 11603
--- @field data {
---     result: string,                                                                             -- 结果
---     worldChat: {
---         id: integer,                                                                            -- ID
---         roleId: integer,                                                                        -- 角色ID
---         roleName: string,                                                                       -- 角色名字
---         type: integer,                                                                          -- 类型
---         message: string,                                                                        -- 消息内容
---     },                                                                                          -- 
--- }

--- @class ChatGetWorldListRequest
--- @field protocol number 11604
--- @field data integer

--- @class ChatGetWorldListRequest
--- @field protocol number 11604
--- @field data {
---     id: integer,                                                                                -- ID
---     roleId: integer,                                                                            -- 角色ID
---     roleName: string,                                                                           -- 角色名字
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息内容
--- }[]

--- @class ChatGuildRequest
--- @field protocol number 11605
--- @field data {
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息
--- }

--- @class ChatGuildRequest
--- @field protocol number 11605
--- @field data {
---     result: string,                                                                             -- 结果
---     guildChat: {
---         id: integer,                                                                            -- ID
---         roleId: integer,                                                                        -- 角色ID
---         roleName: string,                                                                       -- 角色名字
---         type: integer,                                                                          -- 类型
---         message: string,                                                                        -- 消息内容
---     },                                                                                          -- 
--- }

--- @class ChatGetGuildListRequest
--- @field protocol number 11606
--- @field data integer

--- @class ChatGetGuildListRequest
--- @field protocol number 11606
--- @field data {
---     id: integer,                                                                                -- ID
---     roleId: integer,                                                                            -- 角色ID
---     roleName: string,                                                                           -- 角色名字
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息内容
--- }[]

--- @class ChatPrivateRequest
--- @field protocol number 11607
--- @field data {
---     roleId: integer,                                                                            -- 角色ID
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息
--- }

--- @class ChatPrivateRequest
--- @field protocol number 11607
--- @field data {
---     result: string,                                                                             -- 结果
---     privateChat: {
---         senderId: integer,                                                                      -- 发送者角色ID
---         receiverId: integer,                                                                    -- 接收者角色ID
---         type: integer,                                                                          -- 类型
---         message: string,                                                                        -- 消息内容
---     },                                                                                          -- 
--- }

--- @class ChatGetPrivateListRequest
--- @field protocol number 11608
--- @field data {
---     roleId: integer,                                                                            -- 角色ID
---     page: integer,                                                                              -- 页
--- }

--- @class ChatGetPrivateListRequest
--- @field protocol number 11608
--- @field data {
---     senderId: integer,                                                                          -- 发送者角色ID
---     receiverId: integer,                                                                        -- 接收者角色ID
---     type: integer,                                                                              -- 类型
---     message: string,                                                                            -- 消息内容
--- }[]

ChatProtocol = {}

function ChatProtocol.encode(offset, protocol, data)
    if protocol == 11602 then
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data)
        offset = offset + 1
        return table
    elseif protocol == 11603 then
        local table = {}

        -- 类型
        table[offset] = string.pack(">I1", data.type)
        offset = offset + 1
        -- 消息
        table[offset] = string.pack(">s2", data.message)
        offset = offset + 1
        return table
    elseif protocol == 11604 then
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data)
        offset = offset + 1
        return table
    elseif protocol == 11605 then
        local table = {}

        -- 类型
        table[offset] = string.pack(">I1", data.type)
        offset = offset + 1
        -- 消息
        table[offset] = string.pack(">s2", data.message)
        offset = offset + 1
        return table
    elseif protocol == 11606 then
        local table = {}
        -- 页
        table[offset] = string.pack(">I2", data)
        offset = offset + 1
        return table
    elseif protocol == 11607 then
        local table = {}

        -- 角色ID
        table[offset] = string.pack(">I8", data.roleId)
        offset = offset + 1
        -- 类型
        table[offset] = string.pack(">I1", data.type)
        offset = offset + 1
        -- 消息
        table[offset] = string.pack(">s2", data.message)
        offset = offset + 1
        return table
    elseif protocol == 11608 then
        local table = {}

        -- 角色ID
        table[offset] = string.pack(">I8", data.roleId)
        offset = offset + 1
        -- 页
        table[offset] = string.pack(">I2", data.page)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function ChatProtocol.decode(offset, protocol, bytes)
    if protocol == 11602 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local dataDataId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色ID
            local dataDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色名字
            local dataDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataRoleName)
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 消息内容
            local dataDataMessage = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataMessage)
            -- object
            local dataData = {id = dataDataId, roleId = dataDataRoleId, roleName = dataDataRoleName, type = dataDataType, message = dataDataMessage}
            data[dataIndex] = dataData
        end
        return {protocol = 11602, data = data}
    elseif protocol == 11603 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 
        -- ID
        local dataWorldChatId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 角色ID
        local dataWorldChatRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 角色名字
        local dataWorldChatRoleName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataWorldChatRoleName)
        -- 类型
        local dataWorldChatType = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 消息内容
        local dataWorldChatMessage = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataWorldChatMessage)
        -- object
        local dataWorldChat = {id = dataWorldChatId, roleId = dataWorldChatRoleId, roleName = dataWorldChatRoleName, type = dataWorldChatType, message = dataWorldChatMessage}
        -- object
        local data = {result = dataResult, worldChat = dataWorldChat}
        return {protocol = 11603, data = data}
    elseif protocol == 11604 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local dataDataId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色ID
            local dataDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色名字
            local dataDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataRoleName)
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 消息内容
            local dataDataMessage = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataMessage)
            -- object
            local dataData = {id = dataDataId, roleId = dataDataRoleId, roleName = dataDataRoleName, type = dataDataType, message = dataDataMessage}
            data[dataIndex] = dataData
        end
        return {protocol = 11604, data = data}
    elseif protocol == 11605 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 
        -- ID
        local dataGuildChatId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 角色ID
        local dataGuildChatRoleId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 角色名字
        local dataGuildChatRoleName = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataGuildChatRoleName)
        -- 类型
        local dataGuildChatType = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 消息内容
        local dataGuildChatMessage = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataGuildChatMessage)
        -- object
        local dataGuildChat = {id = dataGuildChatId, roleId = dataGuildChatRoleId, roleName = dataGuildChatRoleName, type = dataGuildChatType, message = dataGuildChatMessage}
        -- object
        local data = {result = dataResult, guildChat = dataGuildChat}
        return {protocol = 11605, data = data}
    elseif protocol == 11606 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- ID
            local dataDataId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色ID
            local dataDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色名字
            local dataDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataRoleName)
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 消息内容
            local dataDataMessage = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataMessage)
            -- object
            local dataData = {id = dataDataId, roleId = dataDataRoleId, roleName = dataDataRoleName, type = dataDataType, message = dataDataMessage}
            data[dataIndex] = dataData
        end
        return {protocol = 11606, data = data}
    elseif protocol == 11607 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 
        -- 发送者角色ID
        local dataPrivateChatSenderId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 接收者角色ID
        local dataPrivateChatReceiverId = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 类型
        local dataPrivateChatType = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 消息内容
        local dataPrivateChatMessage = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataPrivateChatMessage)
        -- object
        local dataPrivateChat = {senderId = dataPrivateChatSenderId, receiverId = dataPrivateChatReceiverId, type = dataPrivateChatType, message = dataPrivateChatMessage}
        -- object
        local data = {result = dataResult, privateChat = dataPrivateChat}
        return {protocol = 11607, data = data}
    elseif protocol == 11608 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 发送者角色ID
            local dataDataSenderId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 接收者角色ID
            local dataDataReceiverId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 类型
            local dataDataType = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- 消息内容
            local dataDataMessage = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataMessage)
            -- object
            local dataData = {senderId = dataDataSenderId, receiverId = dataDataReceiverId, type = dataDataType, message = dataDataMessage}
            data[dataIndex] = dataData
        end
        return {protocol = 11608, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end