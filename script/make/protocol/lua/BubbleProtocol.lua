function encodeBubbleProtocol(offset, protocol, data)
    local switch = {

    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeBubbleProtocol(offset, protocol, data)
    local switch = {
        [12101] = function()
            local offset = offset
            -- 气泡列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 气泡ID
                local bubbleId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 过期时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {bubbleId = bubbleId, expireTime = expireTime}
            end
            return {list = list}
        end,
        [12102] = function()
            local offset = offset
            -- 气泡ID列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 气泡ID
                local bubbleId = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {bubbleId = bubbleId}
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