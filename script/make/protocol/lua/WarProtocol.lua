WarProtocol = {}

function WarProtocol.encode(offset, protocol, data)
    if protocol == 18001 then
        local offset = offset
        local table = {}
        -- 怪物Id
        table[offset] = string.pack(">I4", data["monsterId"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function WarProtocol.decode(offset, protocol, data)
    if protocol == 18001 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end