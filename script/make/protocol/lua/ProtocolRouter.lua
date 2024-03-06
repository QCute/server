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

ProtocolRouter = {}

function ProtocolRouter.encode(offset, protocol, data) 
    local number = math.floor(protocol / 100)
    if number == 100 then 
        return AccountProtocol.encode(offset, protocol, data)
    elseif number == 101 then 
        return RoleProtocol.encode(offset, protocol, data)
    elseif number == 111 then 
        return ItemProtocol.encode(offset, protocol, data)
    elseif number == 112 then 
        return TaskProtocol.encode(offset, protocol, data)
    elseif number == 113 then 
        return ShopProtocol.encode(offset, protocol, data)
    elseif number == 114 then 
        return MailProtocol.encode(offset, protocol, data)
    elseif number == 115 then 
        return FriendProtocol.encode(offset, protocol, data)
    elseif number == 116 then 
        return ChatProtocol.encode(offset, protocol, data)
    elseif number == 117 then 
        return SkillProtocol.encode(offset, protocol, data)
    elseif number == 118 then 
        return BuffProtocol.encode(offset, protocol, data)
    elseif number == 119 then 
        return TitleProtocol.encode(offset, protocol, data)
    elseif number == 120 then 
        return FashionProtocol.encode(offset, protocol, data)
    elseif number == 121 then 
        return BubbleProtocol.encode(offset, protocol, data)
    elseif number == 122 then 
        return AchievementProtocol.encode(offset, protocol, data)
    elseif number == 123 then 
        return DailyProtocol.encode(offset, protocol, data)
    elseif number == 150 then 
        return WelfareProtocol.encode(offset, protocol, data)
    elseif number == 161 then 
        return AuctionProtocol.encode(offset, protocol, data)
    elseif number == 170 then 
        return DungeonProtocol.encode(offset, protocol, data)
    elseif number == 180 then 
        return WarProtocol.encode(offset, protocol, data)
    elseif number == 190 then 
        return RankProtocol.encode(offset, protocol, data)
    elseif number == 191 then 
        return RankCenterProtocol.encode(offset, protocol, data)
    elseif number == 192 then 
        return RankWorldProtocol.encode(offset, protocol, data)
    elseif number == 200 then 
        return MapProtocol.encode(offset, protocol, data)
    elseif number == 301 then 
        return GuildProtocol.encode(offset, protocol, data)
    elseif number == 500 then 
        return NoticeProtocol.encode(offset, protocol, data)
    elseif number == 600 then 
        return CheatProtocol.encode(offset, protocol, data)
    elseif number == 655 then 
        return TestProtocol.encode(offset, protocol, data)
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end

function ProtocolRouter.decode(offset, protocol, data) 
    local number = math.floor(protocol / 100)
    if number == 100 then 
        return AccountProtocol.decode(offset, protocol, data)
    elseif number == 101 then 
        return RoleProtocol.decode(offset, protocol, data)
    elseif number == 111 then 
        return ItemProtocol.decode(offset, protocol, data)
    elseif number == 112 then 
        return TaskProtocol.decode(offset, protocol, data)
    elseif number == 113 then 
        return ShopProtocol.decode(offset, protocol, data)
    elseif number == 114 then 
        return MailProtocol.decode(offset, protocol, data)
    elseif number == 115 then 
        return FriendProtocol.decode(offset, protocol, data)
    elseif number == 116 then 
        return ChatProtocol.decode(offset, protocol, data)
    elseif number == 117 then 
        return SkillProtocol.decode(offset, protocol, data)
    elseif number == 118 then 
        return BuffProtocol.decode(offset, protocol, data)
    elseif number == 119 then 
        return TitleProtocol.decode(offset, protocol, data)
    elseif number == 120 then 
        return FashionProtocol.decode(offset, protocol, data)
    elseif number == 121 then 
        return BubbleProtocol.decode(offset, protocol, data)
    elseif number == 122 then 
        return AchievementProtocol.decode(offset, protocol, data)
    elseif number == 123 then 
        return DailyProtocol.decode(offset, protocol, data)
    elseif number == 150 then 
        return WelfareProtocol.decode(offset, protocol, data)
    elseif number == 161 then 
        return AuctionProtocol.decode(offset, protocol, data)
    elseif number == 170 then 
        return DungeonProtocol.decode(offset, protocol, data)
    elseif number == 180 then 
        return WarProtocol.decode(offset, protocol, data)
    elseif number == 190 then 
        return RankProtocol.decode(offset, protocol, data)
    elseif number == 191 then 
        return RankCenterProtocol.decode(offset, protocol, data)
    elseif number == 192 then 
        return RankWorldProtocol.decode(offset, protocol, data)
    elseif number == 200 then 
        return MapProtocol.decode(offset, protocol, data)
    elseif number == 301 then 
        return GuildProtocol.decode(offset, protocol, data)
    elseif number == 500 then 
        return NoticeProtocol.decode(offset, protocol, data)
    elseif number == 600 then 
        return CheatProtocol.decode(offset, protocol, data)
    elseif number == 655 then 
        return TestProtocol.decode(offset, protocol, data)
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end