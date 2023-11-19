--- @class BubbleQueryRequest
--- @field protocol number 12101
--- @field data {
--- }

--- @class BubbleQueryRequest
--- @field protocol number 12101
--- @field data {
---     bubbleId: integer,                                                                          -- 气泡ID
---     expireTime: integer,                                                                        -- 过期时间
--- }[]



--- @class BubbleDeleteRequest
--- @field protocol number 12102
--- @field data integer[]

BubbleProtocol = {}

function BubbleProtocol.encode(offset, protocol, data)
    if protocol == 12101 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function BubbleProtocol.decode(offset, protocol, bytes)
    if protocol == 12101 then
        -- 气泡列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 气泡ID
            local dataDataBubbleId = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 过期时间
            local dataDataExpireTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {bubbleId = dataDataBubbleId, expireTime = dataDataExpireTime}
            data[dataIndex] = dataData
        end
        return {protocol = 12101, data = data}
    elseif protocol == 12102 then
        -- 气泡ID列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 气泡ID
            local dataData = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            data[dataIndex] = dataData
        end
        return {protocol = 12102, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end