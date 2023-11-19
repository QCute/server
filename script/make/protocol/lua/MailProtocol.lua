MailProtocol = {}

function MailProtocol.encode(offset, protocol, data)
    if protocol == 11401 then
        local table = {}

        return table
    elseif protocol == 11402 then
        local table = {}
        -- 邮件ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11403 then
        local table = {}
        -- 邮件ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 11404 then
        local table = {}
        -- 邮件ID
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function MailProtocol.decode(offset, protocol, bytes)
    if protocol == 11401 then
        -- 
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 邮件ID
            local dataDataMailId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 接收时间
            local dataDataReceiveTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 有效时间
            local dataDataExpireTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 读取时间
            local dataDataReadTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 领取附件时间
            local dataDataReceiveAttachmentTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 标题
            local dataDataTitle = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataTitle)
            -- 内容
            local dataDataContent = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataContent)
            -- 附件列表
            local dataDataAttachment = {}
            local dataDataAttachmentLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataDataAttachmentIndex = 1, dataDataAttachmentLength do
                -- 
                -- 物品ID
                local dataDataAttachmentDataItemId = string.unpack(">I4", bytes, offset)
                offset = offset + 4
                -- 数量
                local dataDataAttachmentDataNumber = string.unpack(">I2", bytes, offset)
                offset = offset + 2
                -- object
                local dataDataAttachmentData = {itemId = dataDataAttachmentDataItemId, number = dataDataAttachmentDataNumber}
                dataDataAttachment[dataDataAttachmentIndex] = dataDataAttachmentData
            end
            -- object
            local dataData = {mailId = dataDataMailId, receiveTime = dataDataReceiveTime, expireTime = dataDataExpireTime, readTime = dataDataReadTime, receiveAttachmentTime = dataDataReceiveAttachmentTime, title = dataDataTitle, content = dataDataContent, attachment = dataDataAttachment}
            data[dataIndex] = dataData
        end
        return data
    elseif protocol == 11402 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11403 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 11404 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end