using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public class ProtocolReader
{
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);
    System.IO.BinaryReader reader = new System.IO.BinaryReader(new System.IO.MemoryStream(0));

    public Map Read(System.IO.Stream stream)
    {
        if(stream != null)
        {
            stream = new System.IO.BufferedStream(stream);
            this.reader = new System.IO.BinaryReader(stream);
        }
        if(this.reader == null)
        {
            return null;
        }
        // decode
        var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(this.reader.ReadInt16());
        var protocol = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(this.reader.ReadInt16());
        var package = this.reader.ReadBytes(length);
        var reader = new System.IO.BinaryReader(new System.IO.MemoryStream(package));
        var meta = ProtocolDefine.Get(protocol, "read");
        var result = this.__Read(meta, reader);
        return new Map() { {"protocol", protocol}, {"content", result["content"]} };
    }

    Map __Read(List metadata, System.IO.BinaryReader reader)
    {
        var content = new Map();
        foreach(Map meta in metadata)
        {
            switch ((System.String)meta["type"]) 
            {
                case "u8": 
                {
                    content[(System.String)meta["name"]] = reader.ReadByte();
                } break;
                case "u16": 
                {
                    content[(System.String)meta["name"]] = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                } break;
                case "u32": 
                {
                    content[(System.String)meta["name"]] = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                } break;
                case "u64": 
                {
                    content[(System.String)meta["name"]] = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                } break;
                case "i8": 
                {
                    content[(System.String)meta["name"]] = reader.ReadSByte();
                } break;
                case "i16": 
                {
                    content[(System.String)meta["name"]] = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                } break;
                case "i32": 
                {
                    content[(System.String)meta["name"]] = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                } break;
                case "i64": 
                {
                    content[(System.String)meta["name"]] = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                } break;
                case "f32": 
                {
                    content[(System.String)meta["name"]] = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                } break;
                case "f64": 
                {
                    content[(System.String)meta["name"]] = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                } break;
                case "bool": 
                {
                    content[(System.String)meta["name"]] = reader.ReadByte() != 0;
                } break;
                case "binary": 
                {
                    content[(System.String)meta["name"]] = reader.ReadBytes((System.Int32)meta["explain"]);
                } break;
                case "str":
                case "bst":
                case "rst": 
                {
                    var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    content[(System.String)meta["name"]] = this.encoding.GetString(reader.ReadBytes(strLength));
                } break;
                case "list": 
                {
                    var list = new List();
                    var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var explain = (List)meta["explain"];
                    while (length-- > 0) 
                    {
                        var result = this.__Read(explain, reader);
                        list.Add(result["content"]);
                    }
                    content[(System.String)meta["name"]] = list;
                } break;
                case "map": 
                {
                    var map = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>();
                    var key = (System.String)meta["key"];
                    var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var explain = (List)meta["explain"];
                    while (length-- > 0) 
                    {
                        var result = this.__Read(explain, reader);
                        var sub = (System.Collections.Generic.Dictionary<System.String, System.Object>)result["content"];
                        map[sub[key]] = sub;
                    }
                    content[(System.String)meta["name"]] = map;
                } break;
                default: throw new System.ArgumentException(System.String.Format("unknown meta type: {0}", meta["type"]));
            }
        }
        return new Map() { {"content", content} };
    }
}
