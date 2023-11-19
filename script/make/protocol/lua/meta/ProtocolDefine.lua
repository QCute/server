local accountProtocol = require("./AccountProtocol")
local roleProtocol = require("./RoleProtocol")
local itemProtocol = require("./ItemProtocol")
local taskProtocol = require("./TaskProtocol")
local shopProtocol = require("./ShopProtocol")
local mailProtocol = require("./MailProtocol")
local friendProtocol = require("./FriendProtocol")
local chatProtocol = require("./ChatProtocol")
local skillProtocol = require("./SkillProtocol")
local buffProtocol = require("./BuffProtocol")
local titleProtocol = require("./TitleProtocol")
local fashionProtocol = require("./FashionProtocol")
local bubbleProtocol = require("./BubbleProtocol")
local achievementProtocol = require("./AchievementProtocol")
local dailyProtocol = require("./DailyProtocol")
local welfareProtocol = require("./WelfareProtocol")
local auctionProtocol = require("./AuctionProtocol")
local dungeonProtocol = require("./DungeonProtocol")
local warProtocol = require("./WarProtocol")
local rankProtocol = require("./RankProtocol")
local rankCenterProtocol = require("./RankCenterProtocol")
local rankWorldProtocol = require("./RankWorldProtocol")
local mapProtocol = require("./MapProtocol")
local guildProtocol = require("./GuildProtocol")
local noticeProtocol = require("./NoticeProtocol")
local cheatProtocol = require("./CheatProtocol")
local testProtocol = require("./TestProtocol")

ProtocolDefine = {}

function ProtocolDefine.getRead(protocol, type)
    local number = math.floor(protocol / 100)
    if number == 100 then 
        return accountProtocol[protocol].read
    elseif number == 101 then 
        return roleProtocol[protocol].read
    elseif number == 111 then 
        return itemProtocol[protocol].read
    elseif number == 112 then 
        return taskProtocol[protocol].read
    elseif number == 113 then 
        return shopProtocol[protocol].read
    elseif number == 114 then 
        return mailProtocol[protocol].read
    elseif number == 115 then 
        return friendProtocol[protocol].read
    elseif number == 116 then 
        return chatProtocol[protocol].read
    elseif number == 117 then 
        return skillProtocol[protocol].read
    elseif number == 118 then 
        return buffProtocol[protocol].read
    elseif number == 119 then 
        return titleProtocol[protocol].read
    elseif number == 120 then 
        return fashionProtocol[protocol].read
    elseif number == 121 then 
        return bubbleProtocol[protocol].read
    elseif number == 122 then 
        return achievementProtocol[protocol].read
    elseif number == 123 then 
        return dailyProtocol[protocol].read
    elseif number == 150 then 
        return welfareProtocol[protocol].read
    elseif number == 161 then 
        return auctionProtocol[protocol].read
    elseif number == 170 then 
        return dungeonProtocol[protocol].read
    elseif number == 180 then 
        return warProtocol[protocol].read
    elseif number == 190 then 
        return rankProtocol[protocol].read
    elseif number == 191 then 
        return rankCenterProtocol[protocol].read
    elseif number == 192 then 
        return rankWorldProtocol[protocol].read
    elseif number == 200 then 
        return mapProtocol[protocol].read
    elseif number == 301 then 
        return guildProtocol[protocol].read
    elseif number == 500 then 
        return noticeProtocol[protocol].read
    elseif number == 600 then 
        return cheatProtocol[protocol].read
    elseif number == 655 then 
        return testProtocol[protocol].read
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end

function ProtocolDefine.getWrite(protocol, type)
    local number = math.floor(protocol / 100)
    if number == 100 then 
        return accountProtocol[protocol].write
    elseif number == 101 then 
        return roleProtocol[protocol].write
    elseif number == 111 then 
        return itemProtocol[protocol].write
    elseif number == 112 then 
        return taskProtocol[protocol].write
    elseif number == 113 then 
        return shopProtocol[protocol].write
    elseif number == 114 then 
        return mailProtocol[protocol].write
    elseif number == 115 then 
        return friendProtocol[protocol].write
    elseif number == 116 then 
        return chatProtocol[protocol].write
    elseif number == 117 then 
        return skillProtocol[protocol].write
    elseif number == 118 then 
        return buffProtocol[protocol].write
    elseif number == 119 then 
        return titleProtocol[protocol].write
    elseif number == 120 then 
        return fashionProtocol[protocol].write
    elseif number == 121 then 
        return bubbleProtocol[protocol].write
    elseif number == 122 then 
        return achievementProtocol[protocol].write
    elseif number == 123 then 
        return dailyProtocol[protocol].write
    elseif number == 150 then 
        return welfareProtocol[protocol].write
    elseif number == 161 then 
        return auctionProtocol[protocol].write
    elseif number == 170 then 
        return dungeonProtocol[protocol].write
    elseif number == 180 then 
        return warProtocol[protocol].write
    elseif number == 190 then 
        return rankProtocol[protocol].write
    elseif number == 191 then 
        return rankCenterProtocol[protocol].write
    elseif number == 192 then 
        return rankWorldProtocol[protocol].write
    elseif number == 200 then 
        return mapProtocol[protocol].write
    elseif number == 301 then 
        return guildProtocol[protocol].write
    elseif number == 500 then 
        return noticeProtocol[protocol].write
    elseif number == 600 then 
        return cheatProtocol[protocol].write
    elseif number == 655 then 
        return testProtocol[protocol].write
    else
        error(string.format("unknown protocol define: %d", protocol))
    end
end