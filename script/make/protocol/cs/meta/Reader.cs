using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public class Reader
{
    System.Int32 Length = 0;
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);
    System.IO.MemoryStream stream = new System.IO.MemoryStream(1024);
    System.IO.BinaryReader reader = new System.IO.BinaryReader(new System.IO.MemoryStream(0));
    
    public Reader AppendData(System.ArraySegment<byte> segment)
    {
        this.stream.Position = this.Length;
        this.stream.Write(segment.Array, segment.Offset, segment.Count);
        this.stream.Position = 0;
        this.reader = new System.IO.BinaryReader(this.stream);
        this.Length = this.Length + segment.Count;
        return this;
    }

    public Map Read()
    {
        // @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
        if(this.Length >= 4)
        {
            var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(this.reader.ReadInt16());
            if(this.Length >= 4 + length)
            {
                var protocol = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(this.reader.ReadInt16());
                var packet = this.reader.ReadBytes(length);
                this.Length = this.Length - length - 4;
                var reader = new System.IO.BinaryReader(new System.IO.MemoryStream(packet));
                var meta = ProtocolDefine.GetRead(protocol);
                var result = this.__Read__(meta, reader);
                // update stream buffer
                this.stream.Write(this.stream.GetBuffer(), length + 4, this.Length);
                return new Map() { {"protocol", protocol}, {"data", result["data"]} };
            }
        }
        return null;
    }

    Map __Read__(List metadata, System.IO.BinaryReader reader)
    {
        var data = new Map();
        foreach(Map meta in metadata)
        {
            switch ((System.String)meta["type"]) 
            {
                case "u8": 
                {
                    data[(System.String)meta["name"]] = reader.ReadByte();
                } break;
                case "u16": 
                {
                    data[(System.String)meta["name"]] = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                } break;
                case "u32": 
                {
                    data[(System.String)meta["name"]] = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                } break;
                case "u64": 
                {
                    data[(System.String)meta["name"]] = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                } break;
                case "i8": 
                {
                    data[(System.String)meta["name"]] = reader.ReadSByte();
                } break;
                case "i16": 
                {
                    data[(System.String)meta["name"]] = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                } break;
                case "i32": 
                {
                    data[(System.String)meta["name"]] = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                } break;
                case "i64": 
                {
                    data[(System.String)meta["name"]] = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                } break;
                case "f32": 
                {
                    data[(System.String)meta["name"]] = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                } break;
                case "f64": 
                {
                    data[(System.String)meta["name"]] = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                } break;
                case "bool": 
                {
                    data[(System.String)meta["name"]] = reader.ReadByte() != 0;
                } break;
                case "binary": 
                {
                    data[(System.String)meta["name"]] = reader.ReadBytes((System.Int32)meta["explain"]);
                } break;
                case "str":
                case "bst":
                case "rst": 
                {
                    var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    data[(System.String)meta["name"]] = this.encoding.GetString(reader.ReadBytes(strLength));
                } break;
                case "list": 
                {
                    var list = new List();
                    var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var explain = (List)meta["explain"];
                    while (length-- > 0) 
                    {
                        var result = this.__Read__(explain, reader);
                        list.Add(result["data"]);
                    }
                    data[(System.String)meta["name"]] = list;
                } break;
                case "map": 
                {
                    var map = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>();
                    var key = (System.String)meta["key"];
                    var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var explain = (List)meta["explain"];
                    while (length-- > 0) 
                    {
                        var result = this.__Read__(explain, reader);
                        var sub = (System.Collections.Generic.Dictionary<System.String, System.Object>)result["data"];
                        map[sub[key]] = sub;
                    }
                    data[(System.String)meta["name"]] = map;
                } break;
                default: throw new System.ArgumentException(System.String.Format("unknown meta type: {0}", meta["type"]));
            }
        }
        return new Map() { {"data", data} };
    }
}
