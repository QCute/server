export class BubbleQueryRequest {
    /** @type {number} protocol **/
    protocol = 12101;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class BubbleQueryResponse {
    /** @type {number} protocol **/
    protocol = 12101;
    /**
     * @type {Array<{
     *     bubbleId: number;                                                                        // 气泡ID
     *     expireTime: number;                                                                      // 过期时间
     * }>} data
    **/
    data;
}



export class BubbleDeleteResponse {
    /** @type {number} protocol **/
    protocol = 12102;
    /**
     * @type {Array<number>} data
    **/
    data;
}

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
                    const dataDataBubbleId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const dataDataExpireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"bubbleId": dataDataBubbleId, "expireTime": dataDataExpireTime};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 12101, "data": data};
            }
            case 12102: {
                // 气泡ID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 气泡ID
                    const dataData = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(dataData);
                }
                return {"protocol": 12102, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}