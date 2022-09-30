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

function getReadProtocolDefine(protocol, type)
    local switch = {
        [100] = accountProtocol,
        [101] = roleProtocol,
        [111] = itemProtocol,
        [112] = taskProtocol,
        [113] = shopProtocol,
        [114] = mailProtocol,
        [115] = friendProtocol,
        [116] = chatProtocol,
        [117] = skillProtocol,
        [118] = buffProtocol,
        [119] = titleProtocol,
        [120] = fashionProtocol,
        [121] = bubbleProtocol,
        [122] = achievementProtocol,
        [123] = dailyProtocol,
        [150] = welfareProtocol,
        [161] = auctionProtocol,
        [170] = dungeonProtocol,
        [180] = warProtocol,
        [190] = rankProtocol,
        [191] = rankCenterProtocol,
        [192] = rankWorldProtocol,
        [200] = mapProtocol,
        [301] = guildProtocol,
        [500] = noticeProtocol,
        [600] = cheatProtocol,
        [655] = testProtocol
    }
    local data = switch[math.floor(protocol / 100)]
    if data then
        return data[protocol].read
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end

function getWriteProtocolDefine(protocol, type)
    local switch = {
        [100] = accountProtocol,
        [101] = roleProtocol,
        [111] = itemProtocol,
        [112] = taskProtocol,
        [113] = shopProtocol,
        [114] = mailProtocol,
        [115] = friendProtocol,
        [116] = chatProtocol,
        [117] = skillProtocol,
        [118] = buffProtocol,
        [119] = titleProtocol,
        [120] = fashionProtocol,
        [121] = bubbleProtocol,
        [122] = achievementProtocol,
        [123] = dailyProtocol,
        [150] = welfareProtocol,
        [161] = auctionProtocol,
        [170] = dungeonProtocol,
        [180] = warProtocol,
        [190] = rankProtocol,
        [191] = rankCenterProtocol,
        [192] = rankWorldProtocol,
        [200] = mapProtocol,
        [301] = guildProtocol,
        [500] = noticeProtocol,
        [600] = cheatProtocol,
        [655] = testProtocol
    }
    local data = switch[math.floor(protocol / 100)]
    if data then
        return data[protocol].write
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end