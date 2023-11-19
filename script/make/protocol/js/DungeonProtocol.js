export class DungeonQueryRequest {
    /** @type {number} protocol **/
    protocol = 17001;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class DungeonQueryResponse {
    /** @type {number} protocol **/
    protocol = 17001;
    /**
     * @type {Array<{
     *     dungeonId: number;                                                                       // 副本Id
     *     todayNumber: number;                                                                     // 今天次数
     *     totalNumber: number;                                                                     // 总次数
     * }>} data
    **/
    data;
}

export class DungeonEnterRequest {
    /** @type {number} protocol **/
    protocol = 17002;
    /**
     * @type {number} data
    **/
    data;
}

export class DungeonEnterResponse {
    /** @type {number} protocol **/
    protocol = 17002;
    /**
     * @type {string} data
    **/
    data;
}



export class DungeonStartResponse {
    /** @type {number} protocol **/
    protocol = 17003;
    /**
     * @type {string} data
    **/
    data;
}



export class DungeonOverResponse {
    /** @type {number} protocol **/
    protocol = 17004;
    /**
     * @type {string} data
    **/
    data;
}

export class DungeonInspireRequest {
    /** @type {number} protocol **/
    protocol = 17005;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class DungeonInspireResponse {
    /** @type {number} protocol **/
    protocol = 17005;
    /**
     * @type {string} data
    **/
    data;
}

export default class DungeonProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 17001: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 17002: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 副本Id
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 17005: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 17001: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 副本Id
                    const dataDataDungeonId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 今天次数
                    const dataDataTodayNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 总次数
                    const dataDataTotalNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"dungeonId": dataDataDungeonId, "todayNumber": dataDataTodayNumber, "totalNumber": dataDataTotalNumber};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 17001, "data": data};
            }
            case 17002: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 17002, "data": data};
            }
            case 17003: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 17003, "data": data};
            }
            case 17004: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 17004, "data": data};
            }
            case 17005: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 17005, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}