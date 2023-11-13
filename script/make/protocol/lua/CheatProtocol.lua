CheatProtocol = {}

function CheatProtocol.encode(offset, protocol, data)
    if protocol == 60001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 60002 then
        local offset = offset
        local table = {}
        -- 命令
        table[offset] = string.pack(">s2", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function CheatProtocol.decode(offset, protocol, data)
    if protocol == 60001 then
        local offset = offset
        -- 命令列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 描述
            local description = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(description)
            -- 命令
            local command = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(command)
            -- object
            local item = {description = description, command = command}
            data[dataIndex] = item
        end
        return data
    elseif protocol == 60002 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end