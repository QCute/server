import accountProtocol from "./AccountProtocol.js";
import roleProtocol from "./RoleProtocol.js";
import itemProtocol from "./ItemProtocol.js";
import taskProtocol from "./TaskProtocol.js";
import shopProtocol from "./ShopProtocol.js";
import mailProtocol from "./MailProtocol.js";
import friendProtocol from "./FriendProtocol.js";
import chatProtocol from "./ChatProtocol.js";
import skillProtocol from "./SkillProtocol.js";
import buffProtocol from "./BuffProtocol.js";
import titleProtocol from "./TitleProtocol.js";
import fashionProtocol from "./FashionProtocol.js";
import bubbleProtocol from "./BubbleProtocol.js";
import achievementProtocol from "./AchievementProtocol.js";
import dailyProtocol from "./DailyProtocol.js";
import welfareProtocol from "./WelfareProtocol.js";
import auctionProtocol from "./AuctionProtocol.js";
import dungeonProtocol from "./DungeonProtocol.js";
import warProtocol from "./WarProtocol.js";
import rankProtocol from "./RankProtocol.js";
import rankCenterProtocol from "./RankCenterProtocol.js";
import rankWorldProtocol from "./RankWorldProtocol.js";
import mapProtocol from "./MapProtocol.js";
import guildProtocol from "./GuildProtocol.js";
import noticeProtocol from "./NoticeProtocol.js";
import cheatProtocol from "./CheatProtocol.js";
import testProtocol from "./TestProtocol.js";

export default class ProtocolDefine {
    static getRead(protocol) {
        switch (Math.trunc(protocol / 100)) {
            case 100: return accountProtocol[protocol].read;
            case 101: return roleProtocol[protocol].read;
            case 111: return itemProtocol[protocol].read;
            case 112: return taskProtocol[protocol].read;
            case 113: return shopProtocol[protocol].read;
            case 114: return mailProtocol[protocol].read;
            case 115: return friendProtocol[protocol].read;
            case 116: return chatProtocol[protocol].read;
            case 117: return skillProtocol[protocol].read;
            case 118: return buffProtocol[protocol].read;
            case 119: return titleProtocol[protocol].read;
            case 120: return fashionProtocol[protocol].read;
            case 121: return bubbleProtocol[protocol].read;
            case 122: return achievementProtocol[protocol].read;
            case 123: return dailyProtocol[protocol].read;
            case 150: return welfareProtocol[protocol].read;
            case 161: return auctionProtocol[protocol].read;
            case 170: return dungeonProtocol[protocol].read;
            case 180: return warProtocol[protocol].read;
            case 190: return rankProtocol[protocol].read;
            case 191: return rankCenterProtocol[protocol].read;
            case 192: return rankWorldProtocol[protocol].read;
            case 200: return mapProtocol[protocol].read;
            case 301: return guildProtocol[protocol].read;
            case 500: return noticeProtocol[protocol].read;
            case 600: return cheatProtocol[protocol].read;
            case 655: return testProtocol[protocol].read;
            default:throw("unknown protocol define: " + protocol)
        }
    }

    static getWrite(protocol) {
        switch (Math.trunc(protocol / 100)) {
            case 100: return accountProtocol[protocol].write;
            case 101: return roleProtocol[protocol].write;
            case 111: return itemProtocol[protocol].write;
            case 112: return taskProtocol[protocol].write;
            case 113: return shopProtocol[protocol].write;
            case 114: return mailProtocol[protocol].write;
            case 115: return friendProtocol[protocol].write;
            case 116: return chatProtocol[protocol].write;
            case 117: return skillProtocol[protocol].write;
            case 118: return buffProtocol[protocol].write;
            case 119: return titleProtocol[protocol].write;
            case 120: return fashionProtocol[protocol].write;
            case 121: return bubbleProtocol[protocol].write;
            case 122: return achievementProtocol[protocol].write;
            case 123: return dailyProtocol[protocol].write;
            case 150: return welfareProtocol[protocol].write;
            case 161: return auctionProtocol[protocol].write;
            case 170: return dungeonProtocol[protocol].write;
            case 180: return warProtocol[protocol].write;
            case 190: return rankProtocol[protocol].write;
            case 191: return rankCenterProtocol[protocol].write;
            case 192: return rankWorldProtocol[protocol].write;
            case 200: return mapProtocol[protocol].write;
            case 301: return guildProtocol[protocol].write;
            case 500: return noticeProtocol[protocol].write;
            case 600: return cheatProtocol[protocol].write;
            case 655: return testProtocol[protocol].write;
            default:throw("unknown protocol define: " + protocol)
        }
    }
}