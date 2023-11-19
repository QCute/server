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
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 时装ID
            local fashionId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 过期时间
            local expireTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local fashion = {fashionId = fashionId, expireTime = expireTime}
            data[dataIndex] = fashion
        end
        return data
    elseif protocol == 12002 then
        local offset = offset
        -- 时装ID列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 时装ID
            local item = string.unpack(">I4", data, offset)
            offset = offset + 4
            data[dataIndex] = item
        end
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end