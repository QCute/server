function encodeNoticeProtocol(offset, protocol, data)
    local switch = {

    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeNoticeProtocol(offset, protocol, data)
    local switch = {
        [50001] = function()
            local offset = offset
            -- 公告列表
            local noticeList = {}
            local noticeListLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for noticeListIndex = 1, noticeListLength do
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
                noticeList[noticeListIndex] = {noticeId = noticeId, receiveTime = receiveTime, readTime = readTime, title = title, content = content}
            end
            return {noticeList = noticeList}
        end,
        [50002] = function()
            local offset = offset
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
            return {scope = scope, type = type, title = title, msg = msg}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end