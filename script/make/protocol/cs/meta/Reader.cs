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
                var data = this.__Read__(meta, reader);
                // update stream buffer
                this.stream.Write(this.stream.GetBuffer(), length + 4, this.Length);
                return new Map() { {"protocol", protocol}, {"data", data} };
            }
        }
        return null;
    }

    System.Object __Read__(Map meta, System.IO.BinaryReader reader)
    {
        var type = (System.String)meta["type"];
        switch (type) 
        {
            case "binary": 
            {
                return reader.ReadBytes((System.Int32)meta["explain"]);
            }
            case "bool": 
            {
                return reader.ReadByte() != 0;
            }
            case "u8": 
            {
                return reader.ReadByte();
            }
            case "u16": 
            {
                return (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
            }
            case "u32": 
            {
                return (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
            }
            case "u64": 
            {
                return (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
            }
            case "i8": 
            {
                return reader.ReadSByte();
            }
            case "i16": 
            {
                return System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
            }
            case "i32": 
            {
                return System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
            }
            case "i64": 
            {
                return System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
            }
            case "f32": 
            {
                return System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
            }
            case "f64": 
            {
                return System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
            }
            case "str":
            case "bst":
            case "ast": 
            {
                var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                return this.encoding.GetString(reader.ReadBytes(strLength));
            }
            case "list": 
            {
                if(!meta.ContainsKey("key")) {
                    var list = new List();
                    var explain = (List)meta["explain"];
                    var sub = (Map)explain[0];
                    var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    while (length-- > 0) 
                    {
                        var data = this.__Read__(sub, reader);
                        list.Add(data);
                    }
                    return list;
                } else {
                    var key = (System.String)meta["key"];
                    var map = new System.Collections.Generic.Dictionary<System.Object, System.Object>();
                    var explain = (List)meta["explain"];
                    var sub = (Map)explain[0];
                    var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    while (length-- > 0) 
                    {
                        var data = (System.Collections.Generic.Dictionary<System.String, System.Object>)this.__Read__(sub, reader);
                        // @todo convert key data to spec type
                        var keyData = data[key];
                        map[keyData] = data;
                    }
                    return map;
                }
            }
            case "map": 
            {
                var map = new Map();
                var explain = (List)meta["explain"];
                foreach(Map sub in explain)
                {
                    var name = (System.String)sub["name"];
                    map[name] = this.__Read__(sub, reader);
                }
                return map;
            }
            default: throw new System.ArgumentException(System.String.Format("unknown meta type: {0}", meta["type"]));
        }
    }
}
