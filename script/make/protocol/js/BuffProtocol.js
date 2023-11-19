export class BuffQueryRequest {
    /** @type {number} protocol **/
    protocol = 11801;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class BuffQueryResponse {
    /** @type {number} protocol **/
    protocol = 11801;
    /**
     * @type {Array<{
     *     buffId: number;                                                                          // BuffID
     *     expireTime: number;                                                                      // 结束时间
     *     overlap: number;                                                                         // 叠加数量
     * }>} data
    **/
    data;
}



export class BuffDeleteResponse {
    /** @type {number} protocol **/
    protocol = 11802;
    /**
     * @type {Array<number>} data
    **/
    data;
}

export default class BuffProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11801: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11801: {
                // Buff列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // BuffID
                    const dataDataBuffId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 结束时间
                    const dataDataExpireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 叠加数量
                    const dataDataOverlap = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"buffId": dataDataBuffId, "expireTime": dataDataExpireTime, "overlap": dataDataOverlap};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11801, "data": data};
            }
            case 11802: {
                // BuffID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // BuffID
                    const dataData = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11802, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}