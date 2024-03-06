TitleProtocol = {}

function TitleProtocol.encode(offset, protocol, data)
    if protocol == 11901 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TitleProtocol.decode(offset, protocol, data)
    if protocol == 11901 then
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
    elseif protocol == 11902 then
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
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end