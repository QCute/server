--- @class MailQueryRequest
--- @field protocol number 11401
--- @field data {
--- }

--- @class MailQueryRequest
--- @field protocol number 11401
--- @field data {
---     mailId: integer,                                                                            -- 邮件ID
---     receiveTime: integer,                                                                       -- 接收时间
---     expireTime: integer,                                                                        -- 有效时间
---     readTime: integer,                                                                          -- 读取时间
---     receiveAttachmentTime: integer,                                                             -- 领取附件时间
---     title: string,                                                                              -- 标题
---     content: string,                                                                            -- 内容
---     attachment: {
---         itemId: integer,                                                                        -- 物品ID
---         number: integer,                                                                        -- 数量
---     }[],                                                                                        -- 附件列表
--- }[]

--- @class MailReadRequest
--- @field protocol number 11402
--- @field data integer

--- @class MailReadRequest
--- @field protocol number 11402
--- @field data string

--- @class MailReceiveAttachmentRequest
--- @field protocol number 11403
--- @field data integer

--- @class MailReceiveAttachmentRequest
--- @field protocol number 11403
--- @field data string

--- @class MailDeleteRequest
--- @field protocol number 11404
--- @field data integer

--- @class MailDeleteRequest
--- @field protocol number 11404
--- @field data string

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
        return {protocol = 11401, data = data}
    elseif protocol == 11402 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 11402, data = data}
    elseif protocol == 11403 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 11403, data = data}
    elseif protocol == 11404 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 11404, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end