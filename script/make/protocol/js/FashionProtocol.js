export default class FashionProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12001: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12001: {
                // 时装列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 时装ID
                    const fashionId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const fashion = {"fashionId": fashionId, "expireTime": expireTime};
                    // add
                    data.push(fashion);
                }
                return data;
            }
            case 12002: {
                // 时装ID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 时装ID
                    const item = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(item);
                }
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}