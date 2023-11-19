export class ShopQueryRequest {
    /** @type {number} protocol **/
    protocol = 11301;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class ShopQueryResponse {
    /** @type {number} protocol **/
    protocol = 11301;
    /**
     * @type {Array<{
     *     shopId: number;                                                                          // 商店ID
     *     number: number;                                                                          // 数量
     * }>} data
    **/
    data;
}

export class ShopBuyRequest {
    /** @type {number} protocol **/
    protocol = 11302;
    /**
     * @type {{
     *     shopId: number;                                                                          // 商店ID
     *     number: number;                                                                          // 数量
     * }} data
    **/
    data;
}

export class ShopBuyResponse {
    /** @type {number} protocol **/
    protocol = 11302;
    /**
     * @type {string} data
    **/
    data;
}

export default class ShopProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11301: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11302: {

                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 商店ID
                view.setUint32(offset, data.shopId, false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 数量
                view.setUint16(offset, data.number, false);
                offset = offset + 2;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11301: {
                // 已购买列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 商店ID
                    const dataDataShopId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const dataDataNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"shopId": dataDataShopId, "number": dataDataNumber};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11301, "data": data};
            }
            case 11302: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11302, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}