function encodeWelfareProtocol(offset, protocol, data)
    local switch = {
        [15002] = function()
            local offset = offset
            local table = {}
            -- 兑换码
            table[offset] = string.pack(">s2", data["key"])
            offset = offset + 1
            return table
        end,
        [15003] = function()
            local offset = offset
            local table = {}
            -- 红包编号
            table[offset] = string.pack(">I8", data["luckyMoneyNo"])
            offset = offset + 1
            return table
        end,
        [15004] = function()
            local offset = offset
            local table = {}
            -- 红包编号
            table[offset] = string.pack(">I8", data["luckyMoneyNo"])
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

function decodeWelfareProtocol(offset, protocol, data)
    local switch = {
        [15001] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [15002] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            return {result = result}
        end,
        [15003] = function()
            local offset = offset
            -- 红包编号
            local luckyMoneyNo = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 总金币
            local totalGold = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 总数量
            local totalNumber = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 已经领取人数
            local receiveNumber = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 领取列表
            local receiveList = {}
            local receiveListLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for receiveListIndex = 1, receiveListLength do
                -- 服务器Id
                local serverId = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- 角色Id
                local roleId = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 角色名
                local roleName = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(roleName)
                -- 金币
                local gold = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- 领取时间
                local receiveTime = string.unpack(">I4", data, offset)
                offset = offset + 4
                receiveList[receiveListIndex] = {serverId = serverId, roleId = roleId, roleName = roleName, gold = gold, receiveTime = receiveTime}
            end
            -- 发送时间
            local sendTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            return {luckyMoneyNo = luckyMoneyNo, totalGold = totalGold, totalNumber = totalNumber, receiveNumber = receiveNumber, receiveList = receiveList, sendTime = sendTime}
        end,
        [15004] = function()
            local offset = offset
            -- 结果
            local result = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(result)
            -- 金币
            local gold = string.unpack(">I8", data, offset)
            offset = offset + 8
            return {result = result, gold = gold}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end