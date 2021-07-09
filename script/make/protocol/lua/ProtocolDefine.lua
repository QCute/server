function getProtocolDefine(type, protocol)
    local code = math.floor(protocol / 100)
    if code == 100 then
        return accountProtocol[type][protocol]
    elseif code == 101 then
        return roleProtocol[type][protocol]
    elseif code == 111 then
        return itemProtocol[type][protocol]
    elseif code == 112 then
        return questProtocol[type][protocol]
    elseif code == 113 then
        return shopProtocol[type][protocol]
    elseif code == 114 then
        return mailProtocol[type][protocol]
    elseif code == 115 then
        return friendProtocol[type][protocol]
    elseif code == 116 then
        return chatProtocol[type][protocol]
    elseif code == 117 then
        return skillProtocol[type][protocol]
    elseif code == 118 then
        return buffProtocol[type][protocol]
    elseif code == 119 then
        return titleProtocol[type][protocol]
    elseif code == 120 then
        return fashionProtocol[type][protocol]
    elseif code == 121 then
        return bubbleProtocol[type][protocol]
    elseif code == 150 then
        return welfareProtocol[type][protocol]
    elseif code == 161 then
        return auctionProtocol[type][protocol]
    elseif code == 170 then
        return dungeonProtocol[type][protocol]
    elseif code == 180 then
        return warProtocol[type][protocol]
    elseif code == 190 then
        return rankProtocol[type][protocol]
    elseif code == 191 then
        return rankCenterProtocol[type][protocol]
    elseif code == 192 then
        return rankWorldProtocol[type][protocol]
    elseif code == 200 then
        return mapProtocol[type][protocol]
    elseif code == 301 then
        return guildProtocol[type][protocol]
    elseif code == 500 then
        return noticeProtocol[type][protocol]
    elseif code == 600 then
        return cheatProtocol[type][protocol]
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end