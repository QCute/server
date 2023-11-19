require("./AccountProtocol")
require("./RoleProtocol")
require("./ItemProtocol")
require("./TaskProtocol")
require("./ShopProtocol")
require("./MailProtocol")
require("./FriendProtocol")
require("./ChatProtocol")
require("./SkillProtocol")
require("./BuffProtocol")
require("./TitleProtocol")
require("./FashionProtocol")
require("./BubbleProtocol")
require("./AchievementProtocol")
require("./DailyProtocol")
require("./WelfareProtocol")
require("./AuctionProtocol")
require("./DungeonProtocol")
require("./WarProtocol")
require("./RankProtocol")
require("./RankCenterProtocol")
require("./RankWorldProtocol")
require("./MapProtocol")
require("./GuildProtocol")
require("./NoticeProtocol")
require("./CheatProtocol")
require("./TestProtocol")

function encodeProtocol(offset, protocol, data) 
    local number = math.floor(protocol / 100)
    if number == 100 then 
        return encodeAccountProtocol(offset, protocol, data)
    elseif number == 101 then 
        return encodeRoleProtocol(offset, protocol, data)
    elseif number == 111 then 
        return encodeItemProtocol(offset, protocol, data)
    elseif number == 112 then 
        return encodeTaskProtocol(offset, protocol, data)
    elseif number == 113 then 
        return encodeShopProtocol(offset, protocol, data)
    elseif number == 114 then 
        return encodeMailProtocol(offset, protocol, data)
    elseif number == 115 then 
        return encodeFriendProtocol(offset, protocol, data)
    elseif number == 116 then 
        return encodeChatProtocol(offset, protocol, data)
    elseif number == 117 then 
        return encodeSkillProtocol(offset, protocol, data)
    elseif number == 118 then 
        return encodeBuffProtocol(offset, protocol, data)
    elseif number == 119 then 
        return encodeTitleProtocol(offset, protocol, data)
    elseif number == 120 then 
        return encodeFashionProtocol(offset, protocol, data)
    elseif number == 121 then 
        return encodeBubbleProtocol(offset, protocol, data)
    elseif number == 122 then 
        return encodeAchievementProtocol(offset, protocol, data)
    elseif number == 123 then 
        return encodeDailyProtocol(offset, protocol, data)
    elseif number == 150 then 
        return encodeWelfareProtocol(offset, protocol, data)
    elseif number == 161 then 
        return encodeAuctionProtocol(offset, protocol, data)
    elseif number == 170 then 
        return encodeDungeonProtocol(offset, protocol, data)
    elseif number == 180 then 
        return encodeWarProtocol(offset, protocol, data)
    elseif number == 190 then 
        return encodeRankProtocol(offset, protocol, data)
    elseif number == 191 then 
        return encodeRankCenterProtocol(offset, protocol, data)
    elseif number == 192 then 
        return encodeRankWorldProtocol(offset, protocol, data)
    elseif number == 200 then 
        return encodeMapProtocol(offset, protocol, data)
    elseif number == 301 then 
        return encodeGuildProtocol(offset, protocol, data)
    elseif number == 500 then 
        return encodeNoticeProtocol(offset, protocol, data)
    elseif number == 600 then 
        return encodeCheatProtocol(offset, protocol, data)
    elseif number == 655 then 
        return encodeTestProtocol(offset, protocol, data)
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end

function decodeProtocol(offset, protocol, data) 
    local number = math.floor(protocol / 100)
    if number == 100 then 
        return decodeAccountProtocol(offset, protocol, data)
    elseif number == 101 then 
        return decodeRoleProtocol(offset, protocol, data)
    elseif number == 111 then 
        return decodeItemProtocol(offset, protocol, data)
    elseif number == 112 then 
        return decodeTaskProtocol(offset, protocol, data)
    elseif number == 113 then 
        return decodeShopProtocol(offset, protocol, data)
    elseif number == 114 then 
        return decodeMailProtocol(offset, protocol, data)
    elseif number == 115 then 
        return decodeFriendProtocol(offset, protocol, data)
    elseif number == 116 then 
        return decodeChatProtocol(offset, protocol, data)
    elseif number == 117 then 
        return decodeSkillProtocol(offset, protocol, data)
    elseif number == 118 then 
        return decodeBuffProtocol(offset, protocol, data)
    elseif number == 119 then 
        return decodeTitleProtocol(offset, protocol, data)
    elseif number == 120 then 
        return decodeFashionProtocol(offset, protocol, data)
    elseif number == 121 then 
        return decodeBubbleProtocol(offset, protocol, data)
    elseif number == 122 then 
        return decodeAchievementProtocol(offset, protocol, data)
    elseif number == 123 then 
        return decodeDailyProtocol(offset, protocol, data)
    elseif number == 150 then 
        return decodeWelfareProtocol(offset, protocol, data)
    elseif number == 161 then 
        return decodeAuctionProtocol(offset, protocol, data)
    elseif number == 170 then 
        return decodeDungeonProtocol(offset, protocol, data)
    elseif number == 180 then 
        return decodeWarProtocol(offset, protocol, data)
    elseif number == 190 then 
        return decodeRankProtocol(offset, protocol, data)
    elseif number == 191 then 
        return decodeRankCenterProtocol(offset, protocol, data)
    elseif number == 192 then 
        return decodeRankWorldProtocol(offset, protocol, data)
    elseif number == 200 then 
        return decodeMapProtocol(offset, protocol, data)
    elseif number == 301 then 
        return decodeGuildProtocol(offset, protocol, data)
    elseif number == 500 then 
        return decodeNoticeProtocol(offset, protocol, data)
    elseif number == 600 then 
        return decodeCheatProtocol(offset, protocol, data)
    elseif number == 655 then 
        return decodeTestProtocol(offset, protocol, data)
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end