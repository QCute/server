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
    local switch = {
        [100] = encodeAccountProtocol,
        [101] = encodeRoleProtocol,
        [111] = encodeItemProtocol,
        [112] = encodeTaskProtocol,
        [113] = encodeShopProtocol,
        [114] = encodeMailProtocol,
        [115] = encodeFriendProtocol,
        [116] = encodeChatProtocol,
        [117] = encodeSkillProtocol,
        [118] = encodeBuffProtocol,
        [119] = encodeTitleProtocol,
        [120] = encodeFashionProtocol,
        [121] = encodeBubbleProtocol,
        [122] = encodeAchievementProtocol,
        [123] = encodeDailyProtocol,
        [150] = encodeWelfareProtocol,
        [161] = encodeAuctionProtocol,
        [170] = encodeDungeonProtocol,
        [180] = encodeWarProtocol,
        [190] = encodeRankProtocol,
        [191] = encodeRankCenterProtocol,
        [192] = encodeRankWorldProtocol,
        [200] = encodeMapProtocol,
        [301] = encodeGuildProtocol,
        [500] = encodeNoticeProtocol,
        [600] = encodeCheatProtocol,
        [655] = encodeTestProtocol,
    }
    local method = switch[math.floor(protocol / 100)]
    if method then
        return method(offset, protocol, data)
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end

function decodeProtocol(offset, protocol, data) 
    local switch = {
        [100] = decodeAccountProtocol,
        [101] = decodeRoleProtocol,
        [111] = decodeItemProtocol,
        [112] = decodeTaskProtocol,
        [113] = decodeShopProtocol,
        [114] = decodeMailProtocol,
        [115] = decodeFriendProtocol,
        [116] = decodeChatProtocol,
        [117] = decodeSkillProtocol,
        [118] = decodeBuffProtocol,
        [119] = decodeTitleProtocol,
        [120] = decodeFashionProtocol,
        [121] = decodeBubbleProtocol,
        [122] = decodeAchievementProtocol,
        [123] = decodeDailyProtocol,
        [150] = decodeWelfareProtocol,
        [161] = decodeAuctionProtocol,
        [170] = decodeDungeonProtocol,
        [180] = decodeWarProtocol,
        [190] = decodeRankProtocol,
        [191] = decodeRankCenterProtocol,
        [192] = decodeRankWorldProtocol,
        [200] = decodeMapProtocol,
        [301] = decodeGuildProtocol,
        [500] = decodeNoticeProtocol,
        [600] = decodeCheatProtocol,
        [655] = decodeTestProtocol,
    }
    local method = switch[math.floor(protocol / 100)]
    if method then
        return method(offset, protocol, data)
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end