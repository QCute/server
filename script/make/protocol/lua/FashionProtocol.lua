--- @class FashionQueryRequest
--- @field protocol number 12001
--- @field data {
--- }

--- @class FashionQueryRequest
--- @field protocol number 12001
--- @field data {
---     fashionId: integer,                                                                         -- 时装ID
---     expireTime: integer,                                                                        -- 过期时间
--- }[]



--- @class FashionDeleteRequest
--- @field protocol number 12002
--- @field data integer[]

FashionProtocol = {}

function FashionProtocol.encode(offset, protocol, data)
    if protocol == 12001 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function FashionProtocol.decode(offset, protocol, bytes)
    if protocol == 12001 then
        -- 时装列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 时装ID
            local dataDataFashionId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 过期时间
            local dataDataExpireTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {fashionId = dataDataFashionId, expireTime = dataDataExpireTime}
            data[dataIndex] = dataData
        end
        return {protocol = 12001, data = data}
    elseif protocol == 12002 then
        -- 时装ID列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 时装ID
            local dataData = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            data[dataIndex] = dataData
        end
        return {protocol = 12002, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end