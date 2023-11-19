using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public class Writer
{
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);

    public byte[] Write(System.UInt16 protocol, Map data)
    {
        var stream = new System.IO.MemoryStream(1024);
        var writer = new System.IO.BinaryWriter(stream);
        writer.Seek(4, System.IO.SeekOrigin.Begin);
        var meta = ProtocolDefine.GetWrite(protocol);
        this.__Write__(meta, writer, data);
        var length = stream.Position - 4;
        writer.Seek(0, System.IO.SeekOrigin.Begin);
        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)length));
        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)protocol));
        return stream.ToArray();
    }

    void __Write__(Map meta, System.IO.BinaryWriter writer, System.Object data) 
    {
        var type = (System.String)meta["type"];
        switch (type) 
        {
            case "binary": 
            {
                writer.Write((System.Byte[])data);
            } break;
            case "bool": 
            {
                writer.Write((System.Byte)((System.Boolean)data ? 1 : 0));
            } break;
            case "u8": 
            {
                writer.Write((System.Byte)data);
            } break;
            case "u16": 
            {
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
            } break;
            case "u32": 
            {
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
            } break;
            case "u64": 
            {
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
            } break;
            case "i8": 
            {
                writer.Write((System.SByte)data);
            } break;
            case "i16": 
            {
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data));
            } break;
            case "i32": 
            {
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)data));
            } break;
            case "i64": 
            {
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)data));
            } break;
            case "f32": 
            {
                var bytes = System.BitConverter.GetBytes((System.Single)data);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(bytes);
                writer.Write(bytes);
            } break;
            case "f64": 
            {
                var bytes = System.BitConverter.GetBytes((System.Double)data);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(bytes);
                writer.Write(bytes);
            } break;
            case "bst":
            case "str":
            case "ast": 
            {
                var bytes = encoding.GetBytes((System.String)data);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bytes.Length));
                writer.Write(bytes);
            } break;
            case "list": 
            {
                if(!meta.ContainsKey("key")) {
                    var explain = (List)meta["explain"];
                    var sub = (Map)explain[0];
                    var list = (List)data;
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)list.Count));
                    foreach(var item in list)
                    {
                        this.__Write__(sub, writer, item);
                    }
                } else {
                    var explain = (List)meta["explain"];
                    var sub = (Map)explain[0];
                    var keyList = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)data;
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)keyList.Count));
                    foreach(var item in keyList)
                    {
                        this.__Write__(sub, writer, item.Value);
                    }
                }
            } break;
            case "map": 
            {
                var explain = (List)meta["explain"];
                var map = (Map)data;
                foreach(Map sub in explain)
                {
                    var name = (System.String)sub["name"];
                    this.__Write__(sub, writer, map[name]);
                }
            } break;
            default: throw new System.ArgumentException(System.String.Format("unknown type: {0}", meta["type"]));
        }
    }
}
