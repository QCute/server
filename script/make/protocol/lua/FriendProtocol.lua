function encodeFriendProtocol(offset, protocol, data)
    local switch = {
        [11502] = function()
            local offset = offset
            local table = {}
            -- 好友角色ID
            table[offset] = string.pack(">I8", data["friendRoleId"])
            offset = offset + 1
            return table
        end,
        [11503] = function()
            local offset = offset
            local table = {}
            -- 好友角色ID
            table[offset] = string.pack(">I8", data["friendRoleId"])
            offset = offset + 1
            return table
        end,
        [11504] = function()
            local offset = offset
            local table = {}
            -- 好友角色ID
            table[offset] = string.pack(">I8", data["friendRoleId"])
            offset = offset + 1
            return table
        end,
        [11505] = function()
            local offset = offset
            local table = {}
            -- 好友角色ID
            table[offset] = string.pack(">I8", data["friendRoleId"])
            offset = offset + 1
            return table
        end,
        [11506] = function()
            local offset = offset
            local table = {}
            -- 好友角色ID
            table[offset] = string.pack(">I8", data["friendRoleId"])
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

function decodeFriendProtocol(offset, protocol, data)
    local switch = {
        [11501] = function()
            local offset = offset
            -- 好友列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
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
                list[listIndex] = {friendRoleId = friendRoleId, friendName = friendName, relation = relation, time = time}
            end
            return {list = list}
        end,
        [11502] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [11503] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [11504] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 好友角色ID
            local friendRoleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            return {result = result, friendRoleId = friendRoleId}
        end,
        [11505] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 好友角色ID
            local friendRoleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            return {result = result, friendRoleId = friendRoleId}
        end,
        [11506] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 好友角色ID
            local friendRoleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            return {result = result, friendRoleId = friendRoleId}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end