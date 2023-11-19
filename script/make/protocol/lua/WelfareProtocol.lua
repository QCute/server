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
        table[offset] = string.pack(">s2", data)
        offset = offset + 1
        return table
    elseif protocol == 15003 then
        local offset = offset
        local table = {}
        -- 红包编号
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 15004 then
        local offset = offset
        local table = {}
        -- 红包编号
        table[offset] = string.pack(">I8", data)
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
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 15002 then
        local offset = offset
        -- 结果
        local data = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 15003 then
        local offset = offset
        -- 
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
            -- 
            -- 服务器Id
            local receiveListServerId = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 角色Id
            local receiveListRoleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名
            local receiveListRoleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(receiveListRoleName)
            -- 金币
            local receiveListGold = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 领取时间
            local receiveListTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- object
            local receiveListLuckyMoneyRole = {serverId = receiveListServerId, roleId = receiveListRoleId, roleName = receiveListRoleName, gold = receiveListGold, time = receiveListTime}
            receiveList[receiveListIndex] = receiveListLuckyMoneyRole
        end
        -- 发送时间
        local time = string.unpack(">I4", data, offset)
        offset = offset + 4
        -- object
        local luckyMoney = {luckyMoneyNo = luckyMoneyNo, totalGold = totalGold, totalNumber = totalNumber, receiveNumber = receiveNumber, receiveList = receiveList, time = time}
        return luckyMoney
    elseif protocol == 15004 then
        local offset = offset
        -- 
        -- 结果
        local result = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(result)
        -- 金币
        local gold = string.unpack(">I8", data, offset)
        offset = offset + 8
        -- object
        local data = {result = result, gold = gold}
        return data
    elseif protocol == 15005 then
        local offset = offset
        -- 

        -- object
        local data = {}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end