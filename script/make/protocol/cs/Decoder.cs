public class ProtocolDecoder
{
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);
    System.IO.BinaryReader reader = new System.IO.BinaryReader(new System.IO.MemoryStream(0));

    public System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.IO.Stream stream)
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
        var content = ProtocolRouter.Decode(this.encoding, reader, protocol);
        return new System.Collections.Generic.Dictionary<System.String, System.Object>() { {"protocol", protocol}, {"content", content} };
    }
}
