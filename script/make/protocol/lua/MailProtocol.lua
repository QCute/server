MailProtocol = {}

function MailProtocol.encode(offset, protocol, data)
    if protocol == 11401 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 11402 then
        local offset = offset
        local table = {}
        -- 邮件ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11403 then
        local offset = offset
        local table = {}
        -- 邮件ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11404 then
        local offset = offset
        local table = {}
        -- 邮件ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function MailProtocol.decode(offset, protocol, data)
    if protocol == 11401 then
        local offset = offset
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
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
                -- 
                -- 物品ID
                local attachmentItemId = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- 数量
                local attachmentNumber = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- object
                local attachmentItem = {itemId = attachmentItemId, number = attachmentNumber}
                attachment[attachmentIndex] = attachmentItem
            end
            -- object
            local mail = {mailId = mailId, receiveTime = receiveTime, expireTime = expireTime, readTime = readTime, receiveAttachmentTime = receiveAttachmentTime, title = title, content = content, attachment = attachment}
            data[dataIndex] = mail
        end
        return data
    elseif protocol == 11402 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11403 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11404 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end