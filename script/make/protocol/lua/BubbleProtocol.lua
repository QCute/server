BubbleProtocol = {}

function BubbleProtocol.encode(offset, protocol, data)
    if protocol == 12101 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function BubbleProtocol.decode(offset, protocol, data)
    if protocol == 12101 then
        local offset = offset
        -- 气泡列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Bubble
            -- 气泡ID
            local bubbleId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 过期时间
            local expireTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local bubble = {bubbleId = bubbleId, expireTime = expireTime}
            list[listIndex] = bubble
        end
        return {list = list}
    elseif protocol == 12102 then
        local offset = offset
        -- 气泡ID列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Bubble
            -- 气泡ID
            local bubbleId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local bubble = {bubbleId = bubbleId}
            list[listIndex] = bubble
        end
        return {list = list}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end