NoticeProtocol = {}

function NoticeProtocol.encode(offset, protocol, data)
    if protocol == 50001 then
        local offset = offset
        local table = {}
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function NoticeProtocol.decode(offset, protocol, data)
    if protocol == 50001 then
        local offset = offset
        -- 公告列表
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 公告ID
            local noticeId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 收到时间
            local receiveTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 读取时间
            local readTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 标题
            local title = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(title)
            -- 内容
            local content = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(content)
            -- object
            local noticeRole = {noticeId = noticeId, receiveTime = receiveTime, readTime = readTime, title = title, content = content}
            data[dataIndex] = noticeRole
        end
        return data
    elseif protocol == 50002 then
        local offset = offset
        -- 
        -- 范围
        local scope = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 类型
        local type = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- 标题
        local title = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(title)
        -- 消息
        local msg = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(msg)
        -- object
        local data = {scope = scope, type = type, title = title, msg = msg}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end