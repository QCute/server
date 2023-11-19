--- @class NoticeQueryRequest
--- @field protocol number 50001
--- @field data {
--- }

--- @class NoticeQueryRequest
--- @field protocol number 50001
--- @field data {
---     noticeId: integer,                                                                          -- 公告ID
---     receiveTime: integer,                                                                       -- 收到时间
---     readTime: integer,                                                                          -- 读取时间
---     title: string,                                                                              -- 标题
---     content: string,                                                                            -- 内容
--- }[]



--- @class NoticeBroadcastRequest
--- @field protocol number 50002
--- @field data {
---     scope: integer,                                                                             -- 范围
---     type: integer,                                                                              -- 类型
---     title: string,                                                                              -- 标题
---     msg: string,                                                                                -- 消息
--- }

NoticeProtocol = {}

function NoticeProtocol.encode(offset, protocol, data)
    if protocol == 50001 then
        local table = {}

        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function NoticeProtocol.decode(offset, protocol, bytes)
    if protocol == 50001 then
        -- 公告列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 公告ID
            local dataDataNoticeId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 收到时间
            local dataDataReceiveTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 读取时间
            local dataDataReadTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- 标题
            local dataDataTitle = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataTitle)
            -- 内容
            local dataDataContent = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataContent)
            -- object
            local dataData = {noticeId = dataDataNoticeId, receiveTime = dataDataReceiveTime, readTime = dataDataReadTime, title = dataDataTitle, content = dataDataContent}
            data[dataIndex] = dataData
        end
        return {protocol = 50001, data = data}
    elseif protocol == 50002 then
        -- 
        -- 范围
        local dataScope = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 类型
        local dataType = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- 标题
        local dataTitle = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataTitle)
        -- 消息
        local dataMsg = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataMsg)
        -- object
        local data = {scope = dataScope, type = dataType, title = dataTitle, msg = dataMsg}
        return {protocol = 50002, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end