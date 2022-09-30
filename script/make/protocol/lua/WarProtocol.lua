function encodeWarProtocol(offset, protocol, data)
    local switch = {
        [18001] = function()
            local offset = offset
            local table = {}
            -- 怪物Id
            table[offset] = string.pack(">I4", data["monsterId"])
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

function decodeWarProtocol(offset, protocol, data)
    local switch = {
        [18001] = function()
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