class Test 
{
    public static void Main(System.String[] args)
    {
        TestEncoderDecoder();
        TestNetworkEncoderDecoder();
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

            {"indexList", new System.Collections.ArrayList() {
                new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                    {"listBinary", new byte [] {97, 98, 99, 100, 101, 102}},
                    {"listBoolean", false},
                
                    {"listU8", (System.Byte)1},
                    {"listU16", (System.UInt16)2},
                    {"listU32", (System.UInt32)3},
                    {"listU64", (System.UInt64)4},
                
                    {"listI8", (System.SByte)4},
                    {"listI16", (System.Int16)3},
                    {"listI32", (System.Int32)2},
                    {"listI64", (System.Int64)1},
                
                    {"listF32", (System.Single)1.23},
                    {"listF64", (System.Double)4.56},
                
                    {"listStr", "一23"},
                    {"listBst", "1二三"},
                }
            }},

            {"keyList", new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>() {
                {(System.Object)1, new System.Collections.Generic.Dictionary<System.String, System.Object>() {
                    {"listBinary", new byte [] {97, 98, 99, 100, 101, 102}},
                    {"listBoolean", false},
                    
                    {"listU8", (System.Byte)1},
                    {"listU16", (System.UInt16)2},
                    {"listU32", (System.UInt32)3},
                    {"listU64", (System.UInt64)4},
                    
                    {"listI8", (System.SByte)4},
                    {"listI16", (System.Int16)3},
                    {"listI32", (System.Int32)2},
                    {"listI64", (System.Int64)1},
                    
                    {"listF32", (System.Single)1.23},
                    {"listF64", (System.Double)4.56},
                    
                    {"listStr", "一23"},
                    {"listBst", "1二三"},
                }}
            }}
        }}
    };

    public static void TestEncoderDecoder()
    {
        System.Console.WriteLine(Stringify(packet));
        System.Console.WriteLine();
        var encoder = new Encoder();
        var buffer = encoder.Encode((System.UInt16)packet["protocol"], (System.Collections.Generic.Dictionary<System.String, System.Object>)packet["data"]);
        System.Console.WriteLine(Dump(buffer));
        System.Console.WriteLine();
        var stream = new System.IO.MemoryStream(buffer.Length * 2);
        stream.Position = 0;
        stream.Write(buffer, 0, buffer.Length);
        stream.Write(buffer, 0, buffer.Length);
        buffer = stream.GetBuffer();
        var decoder = new Decoder();
        for(int i = 0; i < buffer.Length; i+=10) 
        {
            // var result = decoder.Decode(new System.ArraySegment<byte>(buffer, i, System.Math.Min(10, buffer.Length - i)));   
            var data = new System.ArraySegment<byte>(buffer, i, System.Math.Min(10, buffer.Length - i));
            var result = decoder.AppendData(data).Decode();
            if(result == null)continue;
            System.Console.WriteLine(Stringify(result));
            // Assert
            System.Diagnostics.Debug.Assert(Stringify(packet) == Stringify(result));
            System.Console.WriteLine();
        }
    }

    public static void TestNetworkEncoderDecoder()
    {

        var socket = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Tcp);
        socket.Connect("127.0.0.1", 33333);

        var encoder = new Encoder();
        var buffer = encoder.Encode((System.UInt16)packet["protocol"], (System.Collections.Generic.Dictionary<System.String, System.Object>)packet["data"]);
        System.Console.WriteLine(Dump(buffer));
        System.Console.WriteLine();
        socket.Send(buffer, 0, (int)buffer.Length, System.Net.Sockets.SocketFlags.None);

        byte[] data = new byte[1024];
        var decoder = new Decoder();
        while (true)
        {
            int count = socket.Receive(data);
            decoder.AppendData(new System.ArraySegment<byte>(data, 0, count));
            while(true)
            {
                var result = decoder.Decode();
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

    public static System.String Stringify(System.Collections.Generic.Dictionary<System.String, System.Object> data)
    {
        var str = "{";
        foreach (var kv in data)
        {
            switch (kv.Value.GetType().ToString())
            {
                case "System.Byte":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Byte)kv.Value);
                }break;
                case "System.UInt16":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.UInt16)kv.Value);
                }break;
                case "System.UInt32":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.UInt32)kv.Value);
                }break;
                case "System.UInt64":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.UInt64)kv.Value);
                }break;
                case "System.SByte":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.SByte)kv.Value);
                }break;
                case "System.Int16":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Int16)kv.Value);
                }break;
                case "System.Int32":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Int32)kv.Value);
                }break;
                case "System.Int64":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Int64)kv.Value);
                }break;
                case "System.Single":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Single)kv.Value);
                }break;
                case "System.Double":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Double)kv.Value);
                }break;
                case "System.Boolean":
                {
                    str += kv.Key + " = " + System.String.Format("{0}", (System.Boolean)kv.Value);
                }break;
                case "System.Byte[]":
                {
                    str += kv.Key + " = " + "[";
                    foreach(var item in (System.Byte[])kv.Value)
                    {
                        str += System.String.Format("{0}", item) + ", ";
                    }
                    str += "]";
                }break;
                case "System.String":
                {
                    str += kv.Key + " = " + "\"" + (System.String)kv.Value + "\"";
                }break;
                case "System.Collections.Generic.Dictionary`2[System.String,System.Object]": 
                {
                    str += kv.Key + " = " + "{" + Stringify((System.Collections.Generic.Dictionary<System.String, System.Object>)kv.Value) + "}";
                }break;
                case "System.Collections.Generic.Dictionary`2[System.Object,System.Collections.Generic.Dictionary`2[System.String,System.Object]]": 
                {
                    str += kv.Key + " = " + "{";
                    foreach(var item in (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)kv.Value)
                    {
                        str += item.Key + " = " + Stringify((System.Collections.Generic.Dictionary<System.String, System.Object>)item.Value) + ", ";
                    }
                    str += "}";
                }break;
                case "System.Collections.ArrayList": 
                {
                    str += kv.Key + " = " + "[";
                    foreach(var item in (System.Collections.ArrayList)kv.Value)
                    {
                        str += Stringify((System.Collections.Generic.Dictionary<System.String, System.Object>)item) + ", ";
                    }
                    str += "]";
                }break;
                default:
                {
                    throw new System.ArgumentException(kv.Value.GetType().ToString());
                }
            }
            str += ", ";
        }
        return str + "}";
    }
}
