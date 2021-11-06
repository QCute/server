function getProtocolDefine(protocol, type)
    local code = math.floor(protocol / 100)
    if code == 100 then
        return accountProtocol[protocol][type]
    elseif code == 101 then
        return roleProtocol[protocol][type]
    elseif code == 111 then
        return itemProtocol[protocol][type]
    elseif code == 112 then
        return taskProtocol[protocol][type]
    elseif code == 113 then
        return shopProtocol[protocol][type]
    elseif code == 114 then
        return mailProtocol[protocol][type]
    elseif code == 115 then
        return friendProtocol[protocol][type]
    elseif code == 116 then
        return chatProtocol[protocol][type]
    elseif code == 117 then
        return skillProtocol[protocol][type]
    elseif code == 118 then
        return buffProtocol[protocol][type]
    elseif code == 119 then
        return titleProtocol[protocol][type]
    elseif code == 120 then
        return fashionProtocol[protocol][type]
    elseif code == 121 then
        return bubbleProtocol[protocol][type]
    elseif code == 122 then
        return achievementProtocol[protocol][type]
    elseif code == 123 then
        return dailyProtocol[protocol][type]
    elseif code == 150 then
        return welfareProtocol[protocol][type]
    elseif code == 161 then
        return auctionProtocol[protocol][type]
    elseif code == 170 then
        return dungeonProtocol[protocol][type]
    elseif code == 180 then
        return warProtocol[protocol][type]
    elseif code == 190 then
        return rankProtocol[protocol][type]
    elseif code == 191 then
        return rankCenterProtocol[protocol][type]
    elseif code == 192 then
        return rankWorldProtocol[protocol][type]
    elseif code == 200 then
        return mapProtocol[protocol][type]
    elseif code == 301 then
        return guildProtocol[protocol][type]
    elseif code == 500 then
        return noticeProtocol[protocol][type]
    elseif code == 600 then
        return cheatProtocol[protocol][type]
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end