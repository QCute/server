export class DailyQueryActiveRequest {
    /** @type {number} protocol **/
    protocol = 12301;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class DailyQueryActiveResponse {
    /** @type {number} protocol **/
    protocol = 12301;
    /**
     * @type {{
     *     stageId: number;                                                                         // 奖励阶段ID
     *     score: number;                                                                           // 活跃度
     * }} data
    **/
    data;
}

export class DailyQueryRequest {
    /** @type {number} protocol **/
    protocol = 12302;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class DailyQueryResponse {
    /** @type {number} protocol **/
    protocol = 12302;
    /**
     * @type {Array<{
     *     dailyId: number;                                                                         // 日常ID
     *     isAward: number;                                                                         // 是否领取奖励
     * }>} data
    **/
    data;
}

export class DailyAwardRequest {
    /** @type {number} protocol **/
    protocol = 12303;
    /**
     * @type {number} data
    **/
    data;
}

export class DailyAwardResponse {
    /** @type {number} protocol **/
    protocol = 12303;
    /**
     * @type {string} data
    **/
    data;
}

export class DailyAwardActiveRequest {
    /** @type {number} protocol **/
    protocol = 12304;
    /**
     * @type {number} data
    **/
    data;
}

export class DailyAwardActiveResponse {
    /** @type {number} protocol **/
    protocol = 12304;
    /**
     * @type {string} data
    **/
    data;
}

export default class DailyProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12301: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 12302: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 12303: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 日常ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 12304: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 阶段ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12301: {
                // 
                // 奖励阶段ID
                const dataStageId = view.getUint32(offset, false);
                offset = offset + 4;
                // 活跃度
                const dataScore = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const data = {"stageId": dataStageId, "score": dataScore};
                return {"protocol": 12301, "data": data};
            }
            case 12302: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 日常ID
                    const dataDataDailyId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 是否领取奖励
                    const dataDataIsAward = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataData = {"dailyId": dataDataDailyId, "isAward": dataDataIsAward};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 12302, "data": data};
            }
            case 12303: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 12303, "data": data};
            }
            case 12304: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 12304, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}