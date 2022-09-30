function encodeBuffProtocol(offset, protocol, data)
    local switch = {

    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeBuffProtocol(offset, protocol, data)
    local switch = {
        [11801] = function()
            local offset = offset
            -- Buff列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- BuffID
                local buffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 结束时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 叠加数量
                local overlap = string.unpack(">I2", data, offset)
                offset = offset + 2
                list[listIndex] = {buffId = buffId, expireTime = expireTime, overlap = overlap}
            end
            return {list = list}
        end,
        [11802] = function()
            local offset = offset
            -- Buff列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- BuffID
                local buffId = string.unpack(">I4", data, offset)
                offset = offset + 4
                list[listIndex] = {buffId = buffId}
            end
            return {list = list}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end