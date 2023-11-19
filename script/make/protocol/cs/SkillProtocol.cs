public class SkillQueryRequest
{
    public System.UInt16 protocol = 11701;
    public Empty data;
}

public class SkillQueryResponse
{
    public System.UInt16 protocol = 11701;
    public System.Collections.Generic.List<(
        System.UInt32 skillId,                                                  // 技能ID
        System.UInt16 level                                                     // 技能等级
    )> data;
}

public class SkillLearnRequest
{
    public System.UInt16 protocol = 11702;
    public System.UInt32 data;
}

public class SkillLearnResponse
{
    public System.UInt16 protocol = 11702;
    public System.String data;
}

public static class SkillProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11701:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11702:
            {
                var data = (System.UInt32)dataRaw;
                // 技能ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11701:
            {
                // 技能列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 skillId, System.UInt16 level)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 技能ID
                    var dataDataSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 技能等级
                    var dataDataLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (skillId: dataDataSkillId, level: dataDataLevel);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11701, data: data);
            }
            case 11702:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11702, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}