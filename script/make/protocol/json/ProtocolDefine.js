function getProtocolDefine(type, protocol) {
    switch (Math.trunc(protocol / 100)) {
        case 100: return accountProtocol[type][protocol];
        case 101: return roleProtocol[type][protocol];
        case 102: return assetProtocol[type][protocol];
        case 103: return vipProtocol[type][protocol];
        case 111: return itemProtocol[type][protocol];
        case 112: return questProtocol[type][protocol];
        case 113: return shopProtocol[type][protocol];
        case 114: return mailProtocol[type][protocol];
        case 115: return friendProtocol[type][protocol];
        case 116: return chatProtocol[type][protocol];
        case 117: return skillProtocol[type][protocol];
        case 118: return buffProtocol[type][protocol];
        case 150: return keyProtocol[type][protocol];
        case 161: return auctionProtocol[type][protocol];
        case 190: return rankProtocol[type][protocol];
        case 200: return mapProtocol[type][protocol];
        case 301: return guildProtocol[type][protocol];
        case 500: return noticeProtocol[type][protocol];
        case 600: return cheatProtocol[type][protocol];
        default:throw("unknown protocol define: " + protocol)
    }
}