export default class TitleProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11901: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11901: {
                // 称号列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 称号ID
                    const titleId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const title = {"titleId": titleId, "expireTime": expireTime};
                    // add
                    data.push(title);
                }
                return data;
            }
            case 11902: {
                // 称号ID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 称号ID
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