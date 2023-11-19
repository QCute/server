--- @class BuffQueryRequest
--- @field protocol number 11801
--- @field data {
--- }

--- @class BuffQueryRequest
--- @field protocol number 11801
--- @field data {
---     buffId: integer,                                                                            -- BuffID
---     expireTime: integer,                                                                        -- 结束时间
---     overlap: integer,                                                                           -- 叠加数量
--- }[]



--- @class BuffDeleteRequest
--- @field protocol number 11802
--- @field data integer[]

BuffProtocol = {}

function BuffProtocol.encode(offset, protocol, data)
    if protocol == 11801 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function BuffProtocol.decode(offset, protocol, bytes)
    if protocol == 11801 then
        -- Buff列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- BuffID
            local dataDataBuffId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 结束时间
            local dataDataExpireTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 叠加数量
            local dataDataOverlap = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- object
            local dataData = {buffId = dataDataBuffId, expireTime = dataDataExpireTime, overlap = dataDataOverlap}
            data[dataIndex] = dataData
        end
        return {protocol = 11801, data = data}
    elseif protocol == 11802 then
        -- BuffID列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- BuffID
            local dataData = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            data[dataIndex] = dataData
        end
        return {protocol = 11802, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end