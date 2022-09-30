using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

class ProtocolDefine
{
    public static List Get(System.UInt16 protocol, System.String type)
    {
        switch (protocol / 100)
        {
            case 100: return (List)(((Map)AccountProtocol.GetMeta()[protocol.ToString()])[type]);
            case 101: return (List)(((Map)RoleProtocol.GetMeta()[protocol.ToString()])[type]);
            case 111: return (List)(((Map)ItemProtocol.GetMeta()[protocol.ToString()])[type]);
            case 112: return (List)(((Map)TaskProtocol.GetMeta()[protocol.ToString()])[type]);
            case 113: return (List)(((Map)ShopProtocol.GetMeta()[protocol.ToString()])[type]);
            case 114: return (List)(((Map)MailProtocol.GetMeta()[protocol.ToString()])[type]);
            case 115: return (List)(((Map)FriendProtocol.GetMeta()[protocol.ToString()])[type]);
            case 116: return (List)(((Map)ChatProtocol.GetMeta()[protocol.ToString()])[type]);
            case 117: return (List)(((Map)SkillProtocol.GetMeta()[protocol.ToString()])[type]);
            case 118: return (List)(((Map)BuffProtocol.GetMeta()[protocol.ToString()])[type]);
            case 119: return (List)(((Map)TitleProtocol.GetMeta()[protocol.ToString()])[type]);
            case 120: return (List)(((Map)FashionProtocol.GetMeta()[protocol.ToString()])[type]);
            case 121: return (List)(((Map)BubbleProtocol.GetMeta()[protocol.ToString()])[type]);
            case 122: return (List)(((Map)AchievementProtocol.GetMeta()[protocol.ToString()])[type]);
            case 123: return (List)(((Map)DailyProtocol.GetMeta()[protocol.ToString()])[type]);
            case 150: return (List)(((Map)WelfareProtocol.GetMeta()[protocol.ToString()])[type]);
            case 161: return (List)(((Map)AuctionProtocol.GetMeta()[protocol.ToString()])[type]);
            case 170: return (List)(((Map)DungeonProtocol.GetMeta()[protocol.ToString()])[type]);
            case 180: return (List)(((Map)WarProtocol.GetMeta()[protocol.ToString()])[type]);
            case 190: return (List)(((Map)RankProtocol.GetMeta()[protocol.ToString()])[type]);
            case 191: return (List)(((Map)RankCenterProtocol.GetMeta()[protocol.ToString()])[type]);
            case 192: return (List)(((Map)RankWorldProtocol.GetMeta()[protocol.ToString()])[type]);
            case 200: return (List)(((Map)MapProtocol.GetMeta()[protocol.ToString()])[type]);
            case 301: return (List)(((Map)GuildProtocol.GetMeta()[protocol.ToString()])[type]);
            case 500: return (List)(((Map)NoticeProtocol.GetMeta()[protocol.ToString()])[type]);
            case 600: return (List)(((Map)CheatProtocol.GetMeta()[protocol.ToString()])[type]);
            case 655: return (List)(((Map)TestProtocol.GetMeta()[protocol.ToString()])[type]);
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}

public static class Cast
{
    public static System.Byte ToUInt8(this object data)
    {
        return (System.Byte)data;
    }
    public static System.UInt16 ToUInt16(this object data)
    {
        return (System.UInt16)data;
    }
    public static System.UInt32 ToUInt32(this object data)
    {
        return (System.UInt32)data;
    }
    public static System.UInt64 ToUInt64(this object data)
    {
        return (System.UInt64)data;
    }

    public static System.SByte ToInt8(this object data)
    {
        return (System.SByte)data;
    }
    public static System.Int16 ToInt16(this object data)
    {
        return (System.Int16)data;
    }
    public static System.Int32 ToInt32(this object data)
    {
        return (System.Int32)data;
    }
    public static System.Int64 ToInt64(this object data)
    {
        return (System.Int64)data;
    }

    public static System.Single ToFloat32(this object data)
    {
        return (System.Single)data;
    }
    public static System.Double ToFloat64(this object data)
    {
        return (System.Double)data;
    }

    public static System.Boolean ToBoolean(this object data)
    {
        return (System.Boolean)data;
    }
    public static System.Byte[] ToBinary(this object data)
    {
        return (System.Byte[])data;
    }

    public static System.String ToString(this object data)
    {
        return (System.String)data;
    }

    public static Map ToMap(this object data)
    {
        return (Map)data;
    }

    public static List ToList(this object data)
    {
        return (List)data;
    }
}