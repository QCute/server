function encodeCheatProtocol(offset, protocol, data)
    if protocol == 60001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 60002 then
        local offset = offset
        local table = {}
        -- 命令
        table[offset] = string.pack(">s2", data["command"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeCheatProtocol(offset, protocol, data)
    if protocol == 60001 then
        local offset = offset
        -- 秘籍列表
        local cheatList = {}
        local cheatListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for cheatListIndex = 1, cheatListLength do
            -- 描述
            local description = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(description)
            -- 命令
            local command = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(command)
            cheatList[cheatListIndex] = {description = description, command = command}
        end
        return {cheatList = cheatList}
    elseif protocol == 60002 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end