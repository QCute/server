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
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 称号ID
            local titleId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 过期时间
            local expireTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local title = {titleId = titleId, expireTime = expireTime}
            data[dataIndex] = title
        end
        return data
    elseif protocol == 11902 then
        local offset = offset
        -- 称号ID列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 称号ID
            local item = string.unpack(">I4", data, offset)
            offset = offset + 4
            data[dataIndex] = item
        end
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end