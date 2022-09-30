import { encodeAccountProtocol, decodeAccountProtocol } from "./AccountProtocol.js";
import { encodeRoleProtocol, decodeRoleProtocol } from "./RoleProtocol.js";
import { encodeItemProtocol, decodeItemProtocol } from "./ItemProtocol.js";
import { encodeTaskProtocol, decodeTaskProtocol } from "./TaskProtocol.js";
import { encodeShopProtocol, decodeShopProtocol } from "./ShopProtocol.js";
import { encodeMailProtocol, decodeMailProtocol } from "./MailProtocol.js";
import { encodeFriendProtocol, decodeFriendProtocol } from "./FriendProtocol.js";
import { encodeChatProtocol, decodeChatProtocol } from "./ChatProtocol.js";
import { encodeSkillProtocol, decodeSkillProtocol } from "./SkillProtocol.js";
import { encodeBuffProtocol, decodeBuffProtocol } from "./BuffProtocol.js";
import { encodeTitleProtocol, decodeTitleProtocol } from "./TitleProtocol.js";
import { encodeFashionProtocol, decodeFashionProtocol } from "./FashionProtocol.js";
import { encodeBubbleProtocol, decodeBubbleProtocol } from "./BubbleProtocol.js";
import { encodeAchievementProtocol, decodeAchievementProtocol } from "./AchievementProtocol.js";
import { encodeDailyProtocol, decodeDailyProtocol } from "./DailyProtocol.js";
import { encodeWelfareProtocol, decodeWelfareProtocol } from "./WelfareProtocol.js";
import { encodeAuctionProtocol, decodeAuctionProtocol } from "./AuctionProtocol.js";
import { encodeDungeonProtocol, decodeDungeonProtocol } from "./DungeonProtocol.js";
import { encodeWarProtocol, decodeWarProtocol } from "./WarProtocol.js";
import { encodeRankProtocol, decodeRankProtocol } from "./RankProtocol.js";
import { encodeRankCenterProtocol, decodeRankCenterProtocol } from "./RankCenterProtocol.js";
import { encodeRankWorldProtocol, decodeRankWorldProtocol } from "./RankWorldProtocol.js";
import { encodeMapProtocol, decodeMapProtocol } from "./MapProtocol.js";
import { encodeGuildProtocol, decodeGuildProtocol } from "./GuildProtocol.js";
import { encodeNoticeProtocol, decodeNoticeProtocol } from "./NoticeProtocol.js";
import { encodeCheatProtocol, decodeCheatProtocol } from "./CheatProtocol.js";
import { encodeTestProtocol, decodeTestProtocol } from "./TestProtocol.js";

export function encodeProtocol(textEncoder, view, offset, protocol, data) {
    switch (Math.trunc(protocol / 100)) {
        case 100: return encodeAccountProtocol(textEncoder, view, offset, protocol, data);
        case 101: return encodeRoleProtocol(textEncoder, view, offset, protocol, data);
        case 111: return encodeItemProtocol(textEncoder, view, offset, protocol, data);
        case 112: return encodeTaskProtocol(textEncoder, view, offset, protocol, data);
        case 113: return encodeShopProtocol(textEncoder, view, offset, protocol, data);
        case 114: return encodeMailProtocol(textEncoder, view, offset, protocol, data);
        case 115: return encodeFriendProtocol(textEncoder, view, offset, protocol, data);
        case 116: return encodeChatProtocol(textEncoder, view, offset, protocol, data);
        case 117: return encodeSkillProtocol(textEncoder, view, offset, protocol, data);
        case 118: return encodeBuffProtocol(textEncoder, view, offset, protocol, data);
        case 119: return encodeTitleProtocol(textEncoder, view, offset, protocol, data);
        case 120: return encodeFashionProtocol(textEncoder, view, offset, protocol, data);
        case 121: return encodeBubbleProtocol(textEncoder, view, offset, protocol, data);
        case 122: return encodeAchievementProtocol(textEncoder, view, offset, protocol, data);
        case 123: return encodeDailyProtocol(textEncoder, view, offset, protocol, data);
        case 150: return encodeWelfareProtocol(textEncoder, view, offset, protocol, data);
        case 161: return encodeAuctionProtocol(textEncoder, view, offset, protocol, data);
        case 170: return encodeDungeonProtocol(textEncoder, view, offset, protocol, data);
        case 180: return encodeWarProtocol(textEncoder, view, offset, protocol, data);
        case 190: return encodeRankProtocol(textEncoder, view, offset, protocol, data);
        case 191: return encodeRankCenterProtocol(textEncoder, view, offset, protocol, data);
        case 192: return encodeRankWorldProtocol(textEncoder, view, offset, protocol, data);
        case 200: return encodeMapProtocol(textEncoder, view, offset, protocol, data);
        case 301: return encodeGuildProtocol(textEncoder, view, offset, protocol, data);
        case 500: return encodeNoticeProtocol(textEncoder, view, offset, protocol, data);
        case 600: return encodeCheatProtocol(textEncoder, view, offset, protocol, data);
        case 655: return encodeTestProtocol(textEncoder, view, offset, protocol, data);
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeProtocol(textDecoder, view, offset, protocol) {
    switch (Math.trunc(protocol / 100)) {
        case 100: return decodeAccountProtocol(textDecoder, view, offset, protocol);
        case 101: return decodeRoleProtocol(textDecoder, view, offset, protocol);
        case 111: return decodeItemProtocol(textDecoder, view, offset, protocol);
        case 112: return decodeTaskProtocol(textDecoder, view, offset, protocol);
        case 113: return decodeShopProtocol(textDecoder, view, offset, protocol);
        case 114: return decodeMailProtocol(textDecoder, view, offset, protocol);
        case 115: return decodeFriendProtocol(textDecoder, view, offset, protocol);
        case 116: return decodeChatProtocol(textDecoder, view, offset, protocol);
        case 117: return decodeSkillProtocol(textDecoder, view, offset, protocol);
        case 118: return decodeBuffProtocol(textDecoder, view, offset, protocol);
        case 119: return decodeTitleProtocol(textDecoder, view, offset, protocol);
        case 120: return decodeFashionProtocol(textDecoder, view, offset, protocol);
        case 121: return decodeBubbleProtocol(textDecoder, view, offset, protocol);
        case 122: return decodeAchievementProtocol(textDecoder, view, offset, protocol);
        case 123: return decodeDailyProtocol(textDecoder, view, offset, protocol);
        case 150: return decodeWelfareProtocol(textDecoder, view, offset, protocol);
        case 161: return decodeAuctionProtocol(textDecoder, view, offset, protocol);
        case 170: return decodeDungeonProtocol(textDecoder, view, offset, protocol);
        case 180: return decodeWarProtocol(textDecoder, view, offset, protocol);
        case 190: return decodeRankProtocol(textDecoder, view, offset, protocol);
        case 191: return decodeRankCenterProtocol(textDecoder, view, offset, protocol);
        case 192: return decodeRankWorldProtocol(textDecoder, view, offset, protocol);
        case 200: return decodeMapProtocol(textDecoder, view, offset, protocol);
        case 301: return decodeGuildProtocol(textDecoder, view, offset, protocol);
        case 500: return decodeNoticeProtocol(textDecoder, view, offset, protocol);
        case 600: return decodeCheatProtocol(textDecoder, view, offset, protocol);
        case 655: return decodeTestProtocol(textDecoder, view, offset, protocol);
        default:throw("unknown protocol define: " + protocol)
    }
}