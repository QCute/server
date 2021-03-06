function getProtocolDefine(type, protocol) {
    switch (Math.trunc(protocol / 100)) {
        case 100: return accountProtocol[type][protocol];
        case 101: return roleProtocol[type][protocol];
        case 111: return itemProtocol[type][protocol];
        case 112: return questProtocol[type][protocol];
        case 113: return shopProtocol[type][protocol];
        case 114: return mailProtocol[type][protocol];
        case 115: return friendProtocol[type][protocol];
        case 116: return chatProtocol[type][protocol];
        case 117: return skillProtocol[type][protocol];
        case 118: return buffProtocol[type][protocol];
        case 119: return titleProtocol[type][protocol];
        case 120: return fashionProtocol[type][protocol];
        case 121: return bubbleProtocol[type][protocol];
        case 150: return welfareProtocol[type][protocol];
        case 161: return auctionProtocol[type][protocol];
        case 170: return dungeonProtocol[type][protocol];
        case 180: return warProtocol[type][protocol];
        case 190: return rankProtocol[type][protocol];
        case 191: return rankCenterProtocol[type][protocol];
        case 192: return rankWorldProtocol[type][protocol];
        case 200: return mapProtocol[type][protocol];
        case 301: return guildProtocol[type][protocol];
        case 500: return noticeProtocol[type][protocol];
        case 600: return cheatProtocol[type][protocol];
        default:throw("unknown protocol define: " + protocol)
    }
}