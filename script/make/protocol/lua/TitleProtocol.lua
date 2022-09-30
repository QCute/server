function encodeTitleProtocol(offset, protocol, data)
    local switch = {

    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeTitleProtocol(offset, protocol, data)
    local switch = {
        [11901] = function()
            local offset = offset
            -- 称号列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 称号ID
                local titleId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {titleId = titleId, expireTime = expireTime}
            end
            return {list = list}
        end,
        [11902] = function()
            local offset = offset
            -- 称号ID列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 称号ID
                local titleId = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {titleId = titleId}
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