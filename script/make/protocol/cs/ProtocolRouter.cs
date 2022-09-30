class ProtocolRouter
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data)
    {
        switch (protocol / 100)
        {
            case 100: AccountProtocol.Encode(encoding, writer, protocol, data);break;
            case 101: RoleProtocol.Encode(encoding, writer, protocol, data);break;
            case 111: ItemProtocol.Encode(encoding, writer, protocol, data);break;
            case 112: TaskProtocol.Encode(encoding, writer, protocol, data);break;
            case 113: ShopProtocol.Encode(encoding, writer, protocol, data);break;
            case 114: MailProtocol.Encode(encoding, writer, protocol, data);break;
            case 115: FriendProtocol.Encode(encoding, writer, protocol, data);break;
            case 116: ChatProtocol.Encode(encoding, writer, protocol, data);break;
            case 117: SkillProtocol.Encode(encoding, writer, protocol, data);break;
            case 118: BuffProtocol.Encode(encoding, writer, protocol, data);break;
            case 119: TitleProtocol.Encode(encoding, writer, protocol, data);break;
            case 120: FashionProtocol.Encode(encoding, writer, protocol, data);break;
            case 121: BubbleProtocol.Encode(encoding, writer, protocol, data);break;
            case 122: AchievementProtocol.Encode(encoding, writer, protocol, data);break;
            case 123: DailyProtocol.Encode(encoding, writer, protocol, data);break;
            case 150: WelfareProtocol.Encode(encoding, writer, protocol, data);break;
            case 161: AuctionProtocol.Encode(encoding, writer, protocol, data);break;
            case 170: DungeonProtocol.Encode(encoding, writer, protocol, data);break;
            case 180: WarProtocol.Encode(encoding, writer, protocol, data);break;
            case 190: RankProtocol.Encode(encoding, writer, protocol, data);break;
            case 191: RankCenterProtocol.Encode(encoding, writer, protocol, data);break;
            case 192: RankWorldProtocol.Encode(encoding, writer, protocol, data);break;
            case 200: MapProtocol.Encode(encoding, writer, protocol, data);break;
            case 301: GuildProtocol.Encode(encoding, writer, protocol, data);break;
            case 500: NoticeProtocol.Encode(encoding, writer, protocol, data);break;
            case 600: CheatProtocol.Encode(encoding, writer, protocol, data);break;
            case 655: TestProtocol.Encode(encoding, writer, protocol, data);break;
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol)
    {
        switch (protocol / 100)
        {
            case 100: return AccountProtocol.Decode(encoding, reader, protocol);
            case 101: return RoleProtocol.Decode(encoding, reader, protocol);
            case 111: return ItemProtocol.Decode(encoding, reader, protocol);
            case 112: return TaskProtocol.Decode(encoding, reader, protocol);
            case 113: return ShopProtocol.Decode(encoding, reader, protocol);
            case 114: return MailProtocol.Decode(encoding, reader, protocol);
            case 115: return FriendProtocol.Decode(encoding, reader, protocol);
            case 116: return ChatProtocol.Decode(encoding, reader, protocol);
            case 117: return SkillProtocol.Decode(encoding, reader, protocol);
            case 118: return BuffProtocol.Decode(encoding, reader, protocol);
            case 119: return TitleProtocol.Decode(encoding, reader, protocol);
            case 120: return FashionProtocol.Decode(encoding, reader, protocol);
            case 121: return BubbleProtocol.Decode(encoding, reader, protocol);
            case 122: return AchievementProtocol.Decode(encoding, reader, protocol);
            case 123: return DailyProtocol.Decode(encoding, reader, protocol);
            case 150: return WelfareProtocol.Decode(encoding, reader, protocol);
            case 161: return AuctionProtocol.Decode(encoding, reader, protocol);
            case 170: return DungeonProtocol.Decode(encoding, reader, protocol);
            case 180: return WarProtocol.Decode(encoding, reader, protocol);
            case 190: return RankProtocol.Decode(encoding, reader, protocol);
            case 191: return RankCenterProtocol.Decode(encoding, reader, protocol);
            case 192: return RankWorldProtocol.Decode(encoding, reader, protocol);
            case 200: return MapProtocol.Decode(encoding, reader, protocol);
            case 301: return GuildProtocol.Decode(encoding, reader, protocol);
            case 500: return NoticeProtocol.Decode(encoding, reader, protocol);
            case 600: return CheatProtocol.Decode(encoding, reader, protocol);
            case 655: return TestProtocol.Decode(encoding, reader, protocol);
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

    public static System.Collections.Generic.Dictionary<System.String, System.Object> ToMap(this object data)
    {
        return (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
    }

    public static System.Collections.ArrayList ToList(this object data)
    {
        return (System.Collections.ArrayList)data;
    }
}