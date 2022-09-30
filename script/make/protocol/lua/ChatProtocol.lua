function encodeChatProtocol(offset, protocol, data)
    local switch = {
        [11602] = function()
            local offset = offset
            local table = {}
            -- 页
            table[offset] = string.pack(">I2", data["page"])
            offset = offset + 1
            return table
        end,
        [11603] = function()
            local offset = offset
            local table = {}
            -- 类型
            table[offset] = string.pack(">I1", data["type"])
            offset = offset + 1
            -- 消息
            table[offset] = string.pack(">s2", data["message"])
            offset = offset + 1
            return table
        end,
        [11604] = function()
            local offset = offset
            local table = {}
            -- 页
            table[offset] = string.pack(">I2", data["page"])
            offset = offset + 1
            return table
        end,
        [11605] = function()
            local offset = offset
            local table = {}
            -- 类型
            table[offset] = string.pack(">I1", data["type"])
            offset = offset + 1
            -- 消息
            table[offset] = string.pack(">s2", data["message"])
            offset = offset + 1
            return table
        end,
        [11606] = function()
            local offset = offset
            local table = {}
            -- 页
            table[offset] = string.pack(">I2", data["page"])
            offset = offset + 1
            return table
        end,
        [11607] = function()
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
        end,
        [11608] = function()
            local offset = offset
            local table = {}
            -- 角色ID
            table[offset] = string.pack(">I8", data["roleId"])
            offset = offset + 1
            -- 页
            table[offset] = string.pack(">I2", data["page"])
            offset = offset + 1
            return table
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeChatProtocol(offset, protocol, data)
    local switch = {
        [11602] = function()
            local offset = offset
            -- 
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
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
                list[listIndex] = {id = id, roleId = roleId, roleName = roleName, type = type, message = message}
            end
            return {list = list}
        end,
        [11603] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
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
            return {result = result, id = id, roleId = roleId, roleName = roleName, type = type, message = message}
        end,
        [11604] = function()
            local offset = offset
            -- 
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
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
                list[listIndex] = {id = id, roleId = roleId, roleName = roleName, type = type, message = message}
            end
            return {list = list}
        end,
        [11605] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
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
            return {result = result, id = id, roleId = roleId, roleName = roleName, type = type, message = message}
        end,
        [11606] = function()
            local offset = offset
            -- 
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
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
                list[listIndex] = {id = id, roleId = roleId, roleName = roleName, type = type, message = message}
            end
            return {list = list}
        end,
        [11607] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
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
            return {result = result, senderId = senderId, receiverId = receiverId, type = type, message = message}
        end,
        [11608] = function()
            local offset = offset
            -- 
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
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
                list[listIndex] = {senderId = senderId, receiverId = receiverId, type = type, message = message}
            end
            return {list = list}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end