import AccountProtocol from "./AccountProtocol.js";
import RoleProtocol from "./RoleProtocol.js";
import ItemProtocol from "./ItemProtocol.js";
import TaskProtocol from "./TaskProtocol.js";
import ShopProtocol from "./ShopProtocol.js";
import MailProtocol from "./MailProtocol.js";
import FriendProtocol from "./FriendProtocol.js";
import ChatProtocol from "./ChatProtocol.js";
import SkillProtocol from "./SkillProtocol.js";
import BuffProtocol from "./BuffProtocol.js";
import TitleProtocol from "./TitleProtocol.js";
import FashionProtocol from "./FashionProtocol.js";
import BubbleProtocol from "./BubbleProtocol.js";
import AchievementProtocol from "./AchievementProtocol.js";
import DailyProtocol from "./DailyProtocol.js";
import WelfareProtocol from "./WelfareProtocol.js";
import AuctionProtocol from "./AuctionProtocol.js";
import DungeonProtocol from "./DungeonProtocol.js";
import WarProtocol from "./WarProtocol.js";
import RankProtocol from "./RankProtocol.js";
import RankCenterProtocol from "./RankCenterProtocol.js";
import RankWorldProtocol from "./RankWorldProtocol.js";
import MapProtocol from "./MapProtocol.js";
import GuildProtocol from "./GuildProtocol.js";
import NoticeProtocol from "./NoticeProtocol.js";
import CheatProtocol from "./CheatProtocol.js";
import TestProtocol from "./TestProtocol.js";

export default class ProtocolRouter {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (Math.trunc(protocol / 100)) {
            case 100: return AccountProtocol.encode(textEncoder, view, offset, protocol, data);
            case 101: return RoleProtocol.encode(textEncoder, view, offset, protocol, data);
            case 111: return ItemProtocol.encode(textEncoder, view, offset, protocol, data);
            case 112: return TaskProtocol.encode(textEncoder, view, offset, protocol, data);
            case 113: return ShopProtocol.encode(textEncoder, view, offset, protocol, data);
            case 114: return MailProtocol.encode(textEncoder, view, offset, protocol, data);
            case 115: return FriendProtocol.encode(textEncoder, view, offset, protocol, data);
            case 116: return ChatProtocol.encode(textEncoder, view, offset, protocol, data);
            case 117: return SkillProtocol.encode(textEncoder, view, offset, protocol, data);
            case 118: return BuffProtocol.encode(textEncoder, view, offset, protocol, data);
            case 119: return TitleProtocol.encode(textEncoder, view, offset, protocol, data);
            case 120: return FashionProtocol.encode(textEncoder, view, offset, protocol, data);
            case 121: return BubbleProtocol.encode(textEncoder, view, offset, protocol, data);
            case 122: return AchievementProtocol.encode(textEncoder, view, offset, protocol, data);
            case 123: return DailyProtocol.encode(textEncoder, view, offset, protocol, data);
            case 150: return WelfareProtocol.encode(textEncoder, view, offset, protocol, data);
            case 161: return AuctionProtocol.encode(textEncoder, view, offset, protocol, data);
            case 170: return DungeonProtocol.encode(textEncoder, view, offset, protocol, data);
            case 180: return WarProtocol.encode(textEncoder, view, offset, protocol, data);
            case 190: return RankProtocol.encode(textEncoder, view, offset, protocol, data);
            case 191: return RankCenterProtocol.encode(textEncoder, view, offset, protocol, data);
            case 192: return RankWorldProtocol.encode(textEncoder, view, offset, protocol, data);
            case 200: return MapProtocol.encode(textEncoder, view, offset, protocol, data);
            case 301: return GuildProtocol.encode(textEncoder, view, offset, protocol, data);
            case 500: return NoticeProtocol.encode(textEncoder, view, offset, protocol, data);
            case 600: return CheatProtocol.encode(textEncoder, view, offset, protocol, data);
            case 655: return TestProtocol.encode(textEncoder, view, offset, protocol, data);
            default:throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (Math.trunc(protocol / 100)) {
            case 100: return AccountProtocol.decode(textDecoder, view, offset, protocol);
            case 101: return RoleProtocol.decode(textDecoder, view, offset, protocol);
            case 111: return ItemProtocol.decode(textDecoder, view, offset, protocol);
            case 112: return TaskProtocol.decode(textDecoder, view, offset, protocol);
            case 113: return ShopProtocol.decode(textDecoder, view, offset, protocol);
            case 114: return MailProtocol.decode(textDecoder, view, offset, protocol);
            case 115: return FriendProtocol.decode(textDecoder, view, offset, protocol);
            case 116: return ChatProtocol.decode(textDecoder, view, offset, protocol);
            case 117: return SkillProtocol.decode(textDecoder, view, offset, protocol);
            case 118: return BuffProtocol.decode(textDecoder, view, offset, protocol);
            case 119: return TitleProtocol.decode(textDecoder, view, offset, protocol);
            case 120: return FashionProtocol.decode(textDecoder, view, offset, protocol);
            case 121: return BubbleProtocol.decode(textDecoder, view, offset, protocol);
            case 122: return AchievementProtocol.decode(textDecoder, view, offset, protocol);
            case 123: return DailyProtocol.decode(textDecoder, view, offset, protocol);
            case 150: return WelfareProtocol.decode(textDecoder, view, offset, protocol);
            case 161: return AuctionProtocol.decode(textDecoder, view, offset, protocol);
            case 170: return DungeonProtocol.decode(textDecoder, view, offset, protocol);
            case 180: return WarProtocol.decode(textDecoder, view, offset, protocol);
            case 190: return RankProtocol.decode(textDecoder, view, offset, protocol);
            case 191: return RankCenterProtocol.decode(textDecoder, view, offset, protocol);
            case 192: return RankWorldProtocol.decode(textDecoder, view, offset, protocol);
            case 200: return MapProtocol.decode(textDecoder, view, offset, protocol);
            case 301: return GuildProtocol.decode(textDecoder, view, offset, protocol);
            case 500: return NoticeProtocol.decode(textDecoder, view, offset, protocol);
            case 600: return CheatProtocol.decode(textDecoder, view, offset, protocol);
            case 655: return TestProtocol.decode(textDecoder, view, offset, protocol);
            default:throw("unknown protocol define: " + protocol)
        }
    }
}