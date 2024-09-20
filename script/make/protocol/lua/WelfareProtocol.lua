WelfareProtocol = {}

function WelfareProtocol.encode(offset, protocol, data)
    if protocol == 15001 then
        local offset = offset
        local table = {}
        return table
    elseif protocol == 15002 then
        local offset = offset
        local table = {}
        -- 兑换码
        table[offset] = string.pack(">s2", data["key"])
        offset = offset + 1
        return table
    elseif protocol == 15003 then
        local offset = offset
        local table = {}
        -- 红包编号
        table[offset] = string.pack(">I8", data["luckyMoneyNo"])
        offset = offset + 1
        return table
    elseif protocol == 15004 then
        local offset = offset
        local table = {}
        -- 红包编号
        table[offset] = string.pack(">I8", data["luckyMoneyNo"])
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function WelfareProtocol.decode(offset, protocol, data)
    if protocol == 15001 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 15002 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        return {result = result}
    elseif protocol == 15003 then
        local offset = offset
        -- LuckyMoney
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
            -- LuckyMoneyRole
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
            -- object
            local luckyMoneyRole = {serverId = serverId, roleId = roleId, roleName = roleName, gold = gold, receiveTime = receiveTime}
            receiveList[receiveListIndex] = luckyMoneyRole
        end
        -- 发送时间
        local sendTime = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- object
        local luckyMoney = {luckyMoneyNo = luckyMoneyNo, totalGold = totalGold, totalNumber = totalNumber, receiveNumber = receiveNumber, receiveList = receiveList, sendTime = sendTime}
        return {luckyMoney = luckyMoney}
    elseif protocol == 15004 then
        local offset = offset
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 金币
        local gold = string.unpack(">I8", data, offset)
        offset = offset + 8
        return {result = result, gold = gold}
    elseif protocol == 15005 then
        local offset = offset
        return {}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end