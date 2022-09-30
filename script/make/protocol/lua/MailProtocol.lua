function encodeMailProtocol(offset, protocol, data)
    local switch = {
        [11402] = function()
            local offset = offset
            local table = {}
            -- 邮件ID
            table[offset] = string.pack(">I8", data["mailId"])
            offset = offset + 1
            return table
        end,
        [11403] = function()
            local offset = offset
            local table = {}
            -- 邮件ID
            table[offset] = string.pack(">I8", data["mailId"])
            offset = offset + 1
            return table
        end,
        [11404] = function()
            local offset = offset
            local table = {}
            -- 邮件ID
            table[offset] = string.pack(">I8", data["mailId"])
            offset = offset + 1
            return table
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeMailProtocol(offset, protocol, data)
    local switch = {
        [11401] = function()
            local offset = offset
            -- 邮件列表
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 邮件ID
                local mailId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 接收时间
                local receiveTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 有效时间
                local expireTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 读取时间
                local readTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 领取附件时间
                local receiveAttachmentTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 标题
                local title = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(title)
                -- 内容
                local content = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(content)
                -- 附件列表
                local attachment = {}
                local attachmentLength = string.unpack(">I2", data, offset)
                offset = offset + 2
                for attachmentIndex = 1, attachmentLength do
                    -- 物品ID
                    local itemId = string.unpack(">I4", data, offset)
                    offset = offset + 4
                    -- 数量
                    local number = string.unpack(">I2", data, offset)
                    offset = offset + 2
                    attachment[attachmentIndex] = {itemId = itemId, number = number}
                end
                list[listIndex] = {mailId = mailId, receiveTime = receiveTime, expireTime = expireTime, readTime = readTime, receiveAttachmentTime = receiveAttachmentTime, title = title, content = content, attachment = attachment}
            end
            return {list = list}
        end,
        [11402] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [11403] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [11404] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end