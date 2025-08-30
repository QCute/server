WelfareProtocol = {}

function WelfareProtocol.encode(offset, protocol, data)
    if protocol == 15001 then
        local table = {}

        return table
    elseif protocol == 15002 then
        local table = {}
        -- 兑换码
        table[offset] = string.pack(">s2", data)
        offset = offset + 1
        return table
    elseif protocol == 15003 then
        local table = {}
        -- 红包编号
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    elseif protocol == 15004 then
        local table = {}
        -- 红包编号
        table[offset] = string.pack(">I8", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function WelfareProtocol.decode(offset, protocol, bytes)
    if protocol == 15001 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 15002 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return data
    elseif protocol == 15003 then
        -- 
        -- 红包编号
        local dataLuckyMoneyNo = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 总金币
        local dataTotalGold = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- 总数量
        local dataTotalNumber = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- 已经领取人数
        local dataReceiveNumber = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        -- 领取列表
        local dataReceiveList = {}
        local dataReceiveListLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataReceiveListIndex = 1, dataReceiveListLength do
            -- 
            -- 服务器Id
            local dataReceiveListDataServerId = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- 角色Id
            local dataReceiveListDataRoleId = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 角色名
            local dataReceiveListDataRoleName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataReceiveListDataRoleName)
            -- 金币
            local dataReceiveListDataGold = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- 领取时间
            local dataReceiveListDataTime = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataReceiveListData = {serverId = dataReceiveListDataServerId, roleId = dataReceiveListDataRoleId, roleName = dataReceiveListDataRoleName, gold = dataReceiveListDataGold, time = dataReceiveListDataTime}
            dataReceiveList[dataReceiveListIndex] = dataReceiveListData
        end
        -- 发送时间
        local dataTime = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- object
        local data = {luckyMoneyNo = dataLuckyMoneyNo, totalGold = dataTotalGold, totalNumber = dataTotalNumber, receiveNumber = dataReceiveNumber, receiveList = dataReceiveList, time = dataTime}
        return data
    elseif protocol == 15004 then
        -- 
        -- 结果
        local dataResult = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataResult)
        -- 金币
        local dataGold = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- object
        local data = {result = dataResult, gold = dataGold}
        return data
    elseif protocol == 15005 then
        -- 

        -- object
        local data = {}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end