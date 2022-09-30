function encodeAccountProtocol(offset, protocol, data)
    local switch = {
        [10001] = function()
            local offset = offset
            local table = {}
            -- 服务器ID
            table[offset] = string.pack(">I2", data["serverId"])
            offset = offset + 1
            -- 账户名
            table[offset] = string.pack(">s2", data["accountName"])
            offset = offset + 1
            return table
        end,
        [10002] = function()
            local offset = offset
            local table = {}
            -- 角色名
            table[offset] = string.pack(">s2", data["roleName"])
            offset = offset + 1
            -- 服务器ID
            table[offset] = string.pack(">I2", data["serverId"])
            offset = offset + 1
            -- 账户名
            table[offset] = string.pack(">s2", data["accountName"])
            offset = offset + 1
            -- 性别
            table[offset] = string.pack(">I1", data["sex"])
            offset = offset + 1
            -- 职业
            table[offset] = string.pack(">I1", data["classes"])
            offset = offset + 1
            -- 渠道
            table[offset] = string.pack(">s2", data["channel"])
            offset = offset + 1
            -- 设备
            table[offset] = string.pack(">s2", data["deviceId"])
            offset = offset + 1
            -- mac地址
            table[offset] = string.pack(">s2", data["mac"])
            offset = offset + 1
            -- 设备类型
            table[offset] = string.pack(">s2", data["deviceType"])
            offset = offset + 1
            return table
        end,
        [10003] = function()
            local offset = offset
            local table = {}
            -- 角色ID
            table[offset] = string.pack(">I8", data["roleId"])
            offset = offset + 1
            -- 角色名
            table[offset] = string.pack(">s2", data["roleName"])
            offset = offset + 1
            -- 服务器ID
            table[offset] = string.pack(">I2", data["serverId"])
            offset = offset + 1
            -- 账户名
            table[offset] = string.pack(">s2", data["accountName"])
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

function decodeAccountProtocol(offset, protocol, data)
    local switch = {
        [10000] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [10001] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 角色名列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 角色ID
                local roleId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 角色名
                local roleName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(roleName)
                list[listIndex] = {roleId = roleId, roleName = roleName}
            end
            return {result = result, list = list}
        end,
        [10002] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 角色ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            return {result = result, roleId = roleId, roleName = roleName}
        end,
        [10003] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [10004] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end