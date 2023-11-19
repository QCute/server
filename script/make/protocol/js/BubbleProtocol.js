export default class BubbleProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12101: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12101: {
                // 气泡列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 气泡ID
                    const bubbleId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const bubble = {"bubbleId": bubbleId, "expireTime": expireTime};
                    // add
                    data.push(bubble);
                }
                return data;
            }
            case 12102: {
                // 气泡ID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 气泡ID
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