WarProtocol = {}

function WarProtocol.encode(offset, protocol, data)
    if protocol == 18001 then
        local table = {}
        -- 怪物Id
        table[offset] = string.pack(">I4", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function WarProtocol.decode(offset, protocol, bytes)
    if protocol == 18001 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end