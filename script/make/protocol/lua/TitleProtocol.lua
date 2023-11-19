--- @class TitleQueryRequest
--- @field protocol number 11901
--- @field data {
--- }

--- @class TitleQueryRequest
--- @field protocol number 11901
--- @field data {
---     titleId: integer,                                                                           -- 称号ID
---     expireTime: integer,                                                                        -- 过期时间
--- }[]



--- @class TitleDeleteRequest
--- @field protocol number 11902
--- @field data integer[]

TitleProtocol = {}

function TitleProtocol.encode(offset, protocol, data)
    if protocol == 11901 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TitleProtocol.decode(offset, protocol, bytes)
    if protocol == 11901 then
        -- 称号列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 称号ID
            local dataDataTitleId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 过期时间
            local dataDataExpireTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {titleId = dataDataTitleId, expireTime = dataDataExpireTime}
            data[dataIndex] = dataData
        end
        return {protocol = 11901, data = data}
    elseif protocol == 11902 then
        -- 称号ID列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 称号ID
            local dataData = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            data[dataIndex] = dataData
        end
        return {protocol = 11902, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end