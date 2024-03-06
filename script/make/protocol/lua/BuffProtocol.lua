BuffProtocol = {}

function BuffProtocol.encode(offset, protocol, data)
    if protocol == 11801 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function BuffProtocol.decode(offset, protocol, data)
    if protocol == 11801 then
        local offset = offset
        -- Buff列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- BuffID
            local buffId = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 结束时间
            local expireTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 叠加数量
            local overlap = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- object
            local buff = {buffId = buffId, expireTime = expireTime, overlap = overlap}
            data[dataIndex] = buff
        end
        return data
    elseif protocol == 11802 then
        local offset = offset
        -- BuffID列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- BuffID
            local item = string.unpack(">I4", data, offset)
            offset = offset + 4
            data[dataIndex] = item
        end
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end