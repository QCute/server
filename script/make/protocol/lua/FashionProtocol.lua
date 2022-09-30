function encodeFashionProtocol(offset, protocol, data)
    local switch = {

    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeFashionProtocol(offset, protocol, data)
    local switch = {
        [12001] = function()
            local offset = offset
            -- 时装列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 时装ID
                local fashionId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {fashionId = fashionId, expireTime = expireTime}
            end
            return {list = list}
        end,
        [12002] = function()
            local offset = offset
            -- 时装ID列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 时装ID
                local fashionId = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {fashionId = fashionId}
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