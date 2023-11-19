export class TitleQueryRequest {
    /** @type {number} protocol **/
    protocol = 11901;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class TitleQueryResponse {
    /** @type {number} protocol **/
    protocol = 11901;
    /**
     * @type {Array<{
     *     titleId: number;                                                                         // 称号ID
     *     expireTime: number;                                                                      // 过期时间
     * }>} data
    **/
    data;
}



export class TitleDeleteResponse {
    /** @type {number} protocol **/
    protocol = 11902;
    /**
     * @type {Array<number>} data
    **/
    data;
}

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
                    const dataDataTitleId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const dataDataExpireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"titleId": dataDataTitleId, "expireTime": dataDataExpireTime};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11901, "data": data};
            }
            case 11902: {
                // 称号ID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 称号ID
                    const dataData = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11902, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}