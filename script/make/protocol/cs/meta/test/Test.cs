class Test 
{
    public static void Main(System.String[] args)
    {
        TestReaderWriter();
        TestNetworkReaderWriter();
    }

    static System.Collections.Generic.Dictionary<System.String, System.Object> packet = new System.Collections.Generic.Dictionary<System.String, System.Object>() {
        {"protocol", (System.UInt16)65535},
        {"data", new System.Collections.Generic.Dictionary<System.String, System.Object>() {
            {"binary", new byte [] {97, 98, 99, 100, 101, 102}},
            {"boolean", true},

            {"u8", (System.Byte)1},
            {"u16", (System.UInt16)2},
            {"u32", (System.UInt32)3},
            {"u64", (System.UInt64)4},

            {"i8", (System.SByte)4},
            {"i16", (System.Int16)3},
            {"i32", (System.Int32)2},
            {"i64", (System.Int64)1},

            {"f32", (System.Single)1.23},
            {"f64", (System.Double)4.56},

            {"str", "一23"},
            {"bst", "1二三"},

            {"tuple", new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                {"binary", new byte [] {97, 98, 99, 100, 101, 102}},
                {"sub", new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                    {"u8", (System.Byte)95},
                    {"str", "xyz"},
                }},
                {"list", new System.Collections.Generic.List<System.Object>() {
                    new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", (System.Int16)456}, {"bst", "wow"}},
                    new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", (System.Int16)369}, {"bst", "oops"}},
                }},
                {"single", new System.Collections.Generic.List<System.Object>() {true, false, false, true, false}},
            }},
            
            {"indexList", new System.Collections.Generic.List<System.Object>() {
                new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                    {"binary", new byte [] {97, 98, 99, 100, 101, 102}},
                    {"sub", new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                        {"u8", (System.Byte)95},
                        {"str", "xyz"},
                    }},
                    {"list", new System.Collections.Generic.List<System.Object>() {
                        new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", (System.Int16)456}, {"bst", "wow"}},
                        new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", (System.Int16)369}, {"bst", "oops"}},
                    }},
                    {"single", new System.Collections.Generic.List<System.Object>() {true, false, false, true, false}},
                }
            }},

            {"keyList", new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>() {
                {(System.Object)1, new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                    {"binary", new byte [] {97, 98, 99, 100, 101, 102}},
                    {"boolean", true},

                    {"u8", (System.Byte)1},
                    {"u16", (System.UInt16)2},
                    {"u32", (System.UInt32)3},
                    {"u64", (System.UInt64)4},

                    {"i8", (System.SByte)4},
                    {"i16", (System.Int16)3},
                    {"i32", (System.Int32)2},
                    {"i64", (System.Int64)1},

                    {"f32", (System.Single)1.23},
                    {"f64", (System.Double)4.56},

                    {"str", "一23"},
                    {"bst", "1二三"},
                }}
            }}
        }}
    };

    public static void TestReaderWriter()
    {
        System.Console.WriteLine(Stringify(packet));
        System.Console.WriteLine();
        var writer = new Writer();
        var buffer = writer.Write((System.UInt16)packet["protocol"], (System.Collections.Generic.Dictionary<System.String, System.Object>)packet["data"]);
        System.Console.WriteLine(Dump(buffer));
        System.Console.WriteLine();
        var stream = new System.IO.MemoryStream(buffer.Length * 2);
        stream.Position = 0;
        stream.Write(buffer, 0, buffer.Length);
        stream.Write(buffer, 0, buffer.Length);
        buffer = stream.GetBuffer();
        var reader = new Reader();
        for(int i = 0; i < buffer.Length; i+=10) 
        {
            var data = new System.ArraySegment<byte>(buffer, i, System.Math.Min(10, buffer.Length - i));
            var result = reader.AppendData(data).Read();
            if(result == null)continue;
            System.Console.WriteLine(Stringify(result));
            System.Console.WriteLine();
            // Assert
            System.Diagnostics.Debug.Assert(Stringify(packet) == Stringify(result));
        }
    }
    
    public static void TestNetworkReaderWriter()
    {
        var socket = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Tcp);
        socket.Connect("127.0.0.1", 33333);

        var writer = new Writer();
        var buffer = writer.Write((System.UInt16)packet["protocol"], (System.Collections.Generic.Dictionary<System.String, System.Object>)packet["data"]);
        System.Console.WriteLine(Dump(buffer));
        System.Console.WriteLine();
        socket.Send(buffer, 0, (int)buffer.Length, System.Net.Sockets.SocketFlags.None);

        byte[] data = new byte[1024];
        var reader = new Reader();
        while (true)
        {
            int count = socket.Receive(data);
            reader.AppendData(new System.ArraySegment<byte>(data, 0, count));
            while(true)
            {
                var result = reader.Read();
                if(result == null)break;
                System.Console.WriteLine(Stringify(result));
            }
        }
    }

    public static System.String Dump(byte[] data)
    {
        var str = "[";
        foreach (var i in data)
        {
            str = str + i + ", ";
        }
        return str + "]";
    }

    public static System.String Stringify(System.Object data)
    {
        switch (data.GetType().ToString()) 
        {
            case "System.Byte":
            {
                return System.String.Format("{0}", (System.Byte)data);
            }
            case "System.UInt16":
            {
                return System.String.Format("{0}", (System.UInt16)data);
            }
            case "System.UInt32":
            {
                return System.String.Format("{0}", (System.UInt32)data);
            }
            case "System.UInt64":
            {
                return System.String.Format("{0}", (System.UInt64)data);
            }
            case "System.SByte":
            {
                return System.String.Format("{0}", (System.SByte)data);
            }
            case "System.Int16":
            {
                return System.String.Format("{0}", (System.Int16)data);
            }
            case "System.Int32":
            {
                return System.String.Format("{0}", (System.Int32)data);
            }
            case "System.Int64":
            {
                return System.String.Format("{0}", (System.Int64)data);
            }
            case "System.Single":
            {
                return System.String.Format("{0}", (System.Single)data);
            }
            case "System.Double":
            {
                return System.String.Format("{0}", (System.Double)data);
            }
            case "System.Boolean":
            {
                return System.String.Format("{0}", (System.Boolean)data);
            }
            case "System.Byte[]":
            {
                var str = "[";
                foreach(var item in (System.Byte[])data)
                {
                    str += System.String.Format("{0}", item) + ", ";
                }
                return str += "]";
            }
            case "System.String":
            {
                return "\"" + (System.String)data + "\"";
            }
            case "System.Collections.Generic.List`1[System.Object]": 
            {
                var str = "[";
                foreach(var item in (System.Collections.Generic.List<System.Object>)data)
                {
                    str += Stringify(item) + ", ";
                }
                return str += "]";
            }
            case "System.Collections.Generic.Dictionary`2[System.String,System.Object]": 
            {
                var str = "{";
                foreach (var kv in (System.Collections.Generic.Dictionary<System.String, System.Object>)data)
                {
                    str += kv.Key + " = " + Stringify(kv.Value) + ", ";
                }
                return str += "}";
            }
            case "System.Collections.Generic.Dictionary`2[System.Object,System.Object]": 
            {
                var str = "{";
                foreach (var kv in (System.Collections.Generic.Dictionary<System.Object, System.Object>)data)
                {
                    str += Stringify(kv.Key) + " = " + Stringify(kv.Value) + ", ";
                }
                return str += "}";
            }
            case "System.Collections.Generic.Dictionary`2[System.Object,System.Collections.Generic.Dictionary`2[System.String,System.Object]]": 
            {
                var str = "{";
                foreach(var item in (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)data)
                {
                    str += Stringify(item.Key) + " = " + Stringify(item.Value) + ", ";
                }
                return str += "}";
            }
            default:
            {
                throw new System.ArgumentException(data.GetType().ToString());
            }
        }
    }
}
