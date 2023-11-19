export class AchievementQueryCountRequest {
    /** @type {number} protocol **/
    protocol = 12201;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class AchievementQueryCountResponse {
    /** @type {number} protocol **/
    protocol = 12201;
    /**
     * @type {Array<{
     *     type: number;                                                                            // 统计类型
     *     totalNumber: number;                                                                     // 总数
     * }>} data
    **/
    data;
}

export class AchievementQueryRequest {
    /** @type {number} protocol **/
    protocol = 12202;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class AchievementQueryResponse {
    /** @type {number} protocol **/
    protocol = 12202;
    /**
     * @type {Array<{
     *     achievementId: number;                                                                   // 成就ID
     *     type: number;                                                                            // 成就类型
     * }>} data
    **/
    data;
}

export class AchievementAwardRequest {
    /** @type {number} protocol **/
    protocol = 12203;
    /**
     * @type {number} data
    **/
    data;
}

export class AchievementAwardResponse {
    /** @type {number} protocol **/
    protocol = 12203;
    /**
     * @type {string} data
    **/
    data;
}

export default class AchievementProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12201: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 12202: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 12203: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 成就ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12201: {
                // 统计列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 统计类型
                    const dataDataType = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 总数
                    const dataDataTotalNumber = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"type": dataDataType, "totalNumber": dataDataTotalNumber};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 12201, "data": data};
            }
            case 12202: {
                // 成就列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 成就ID
                    const dataDataAchievementId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 成就类型
                    const dataDataType = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"achievementId": dataDataAchievementId, "type": dataDataType};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 12202, "data": data};
            }
            case 12203: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 12203, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}