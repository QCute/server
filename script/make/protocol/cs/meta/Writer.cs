using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public class ProtocolWriter
{
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);

    public byte[] Write(System.UInt16 protocol, Map data)
    {
        var stream = new System.IO.MemoryStream(1024);
        var writer = new System.IO.BinaryWriter(stream);
        writer.Seek(4, System.IO.SeekOrigin.Begin);
        var meta = ProtocolDefine.Get(protocol, "write");
        this.__Write(meta, writer, data);
        var length = stream.Position - 4;
        writer.Seek(0, System.IO.SeekOrigin.Begin);
        writer.Write((System.UInt16)length);
        writer.Write((System.UInt16)protocol);
        return stream.ToArray();
    }

    void __Write(List metadata, System.IO.BinaryWriter writer, Map data) 
    {
        foreach (Map meta in metadata) 
        {
            switch ((System.String)meta["type"]) 
            {
                case "u8": 
                {
                    writer.Write((System.Byte)data[(System.String)meta["name"]]);
                } break;
                case "u16": 
                {
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data[(System.String)meta["name"]]));
                } break;
                case "u32": 
                {
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data[(System.String)meta["name"]]));
                } break;
                case "u64": 
                {
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data[(System.String)meta["name"]]));
                } break;
                case "i8": 
                {
                    writer.Write((System.SByte)data[(System.String)meta["name"]]);
                } break;
                case "i16": 
                {
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data[(System.String)meta["name"]]));
                } break;
                case "i32": 
                {
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)data[(System.String)meta["name"]]));
                } break;
                case "i64": 
                {
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)data[(System.String)meta["name"]]));
                } break;
                case "f32": 
                {
                    var bytes = System.BitConverter.GetBytes((System.Single)data[(System.String)meta["name"]]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(bytes);
                    writer.Write(bytes);
                } break;
                case "f64": 
                {
                    var bytes = System.BitConverter.GetBytes((System.Double)data[(System.String)meta["name"]]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(bytes);
                    writer.Write(bytes);
                } break;
                case "bool": 
                {
                    writer.Write((System.Byte)((System.Boolean)data[(System.String)meta["name"]] ? 1 : 0));
                } break;
                case "binary": 
                {
                    writer.Write((System.Byte[])data[(System.String)meta["name"]]);
                } break;
                case "bst":
                case "str":
                case "rst": 
                {
                    var bytes = encoding.GetBytes((System.String)data[(System.String)meta["name"]]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bytes.Length));
                    writer.Write(bytes);
                } break;
                case "list": 
                {
                    var explain = (List)meta["explain"];
                    var dataList = (List)data[(System.String)meta["name"]];
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataList.Count));
                    foreach(Map item in dataList)
                    {
                        this.__Write(explain, writer, item);
                    }
                } break;
                case "map": 
                {
                    var explain = (List)meta["explain"];
                    var dataList = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)data[(System.String)meta["name"]];
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataList.Count));
                    foreach(System.Collections.Generic.KeyValuePair<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>> item in dataList)
                    {
                        this.__Write(explain, writer, item.Value);
                    }
                } break;
                default: throw new System.ArgumentException(System.String.Format("unknown type: {0}", meta["type"]));
            }
        }
    }
}
