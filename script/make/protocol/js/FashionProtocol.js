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
                    const dataDataFashionId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const dataDataExpireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"fashionId": dataDataFashionId, "expireTime": dataDataExpireTime};
                    // add
                    data.push(dataData);
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
                    const dataData = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(dataData);
                }
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}