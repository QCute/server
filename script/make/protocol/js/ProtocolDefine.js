function getProtocolDefine(protocol, type) {
    switch (Math.trunc(protocol / 100)) {
        case 100: return accountProtocol[protocol][type];
        case 101: return roleProtocol[protocol][type];
        case 111: return itemProtocol[protocol][type];
        case 112: return taskProtocol[protocol][type];
        case 113: return shopProtocol[protocol][type];
        case 114: return mailProtocol[protocol][type];
        case 115: return friendProtocol[protocol][type];
        case 116: return chatProtocol[protocol][type];
        case 117: return skillProtocol[protocol][type];
        case 118: return buffProtocol[protocol][type];
        case 119: return titleProtocol[protocol][type];
        case 120: return fashionProtocol[protocol][type];
        case 121: return bubbleProtocol[protocol][type];
        case 122: return achievementProtocol[protocol][type];
        case 123: return dailyProtocol[protocol][type];
        case 150: return welfareProtocol[protocol][type];
        case 161: return auctionProtocol[protocol][type];
        case 170: return dungeonProtocol[protocol][type];
        case 180: return warProtocol[protocol][type];
        case 190: return rankProtocol[protocol][type];
        case 191: return rankCenterProtocol[protocol][type];
        case 192: return rankWorldProtocol[protocol][type];
        case 200: return mapProtocol[protocol][type];
        case 301: return guildProtocol[protocol][type];
        case 500: return noticeProtocol[protocol][type];
        case 600: return cheatProtocol[protocol][type];
        default:throw("unknown protocol define: " + protocol)
    }
}