FashionProtocol = {}

function FashionProtocol.encode(offset, protocol, data)
    if protocol == 12001 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function FashionProtocol.decode(offset, protocol, data)
    if protocol == 12001 then
        local offset = offset
        -- 时装列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Fashion
            -- 时装ID
            local fashionId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 过期时间
            local expireTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local fashion = {fashionId = fashionId, expireTime = expireTime}
            list[listIndex] = fashion
        end
        return {list = list}
    elseif protocol == 12002 then
        local offset = offset
        -- 时装ID列表
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- Fashion
            -- 时装ID
            local fashionId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local fashion = {fashionId = fashionId}
            list[listIndex] = fashion
        end
        return {list = list}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end