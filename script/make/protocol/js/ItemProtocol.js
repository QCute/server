export class ItemQueryItemRequest {
    /** @type {number} protocol **/
    protocol = 11101;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class ItemQueryItemResponse {
    /** @type {number} protocol **/
    protocol = 11101;
    /**
     * @type {Array<{
     *     itemNo: BigInt;                                                                          // 物品编号
     *     itemId: number;                                                                          // 物品ID
     *     type: number;                                                                            // 类型
     *     number: number;                                                                          // 数量
     * }>} data
    **/
    data;
}

export class ItemQueryBagRequest {
    /** @type {number} protocol **/
    protocol = 11102;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class ItemQueryBagResponse {
    /** @type {number} protocol **/
    protocol = 11102;
    /**
     * @type {Array<{
     *     itemNo: BigInt;                                                                          // 物品编号
     *     itemId: number;                                                                          // 物品ID
     *     type: number;                                                                            // 类型
     *     number: number;                                                                          // 数量
     * }>} data
    **/
    data;
}

export class ItemQueryStoreRequest {
    /** @type {number} protocol **/
    protocol = 11103;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class ItemQueryStoreResponse {
    /** @type {number} protocol **/
    protocol = 11103;
    /**
     * @type {Array<{
     *     itemNo: BigInt;                                                                          // 物品编号
     *     itemId: number;                                                                          // 物品ID
     *     type: number;                                                                            // 类型
     *     number: number;                                                                          // 数量
     * }>} data
    **/
    data;
}



export class ItemDeleteResponse {
    /** @type {number} protocol **/
    protocol = 11104;
    /**
     * @type {Array<{
     *     itemNo: BigInt;                                                                          // 物品编号
     *     itemId: number;                                                                          // 物品ID
     *     type: number;                                                                            // 类型
     * }>} data
    **/
    data;
}

export class ItemUseRequest {
    /** @type {number} protocol **/
    protocol = 11106;
    /**
     * @type {{
     *     itemNo: BigInt;                                                                          // 物品编号
     *     number: number;                                                                          // 数量
     *     type: number;                                                                            // 类型
     * }} data
    **/
    data;
}

export class ItemUseResponse {
    /** @type {number} protocol **/
    protocol = 11106;
    /**
     * @type {string} data
    **/
    data;
}

export default class ItemProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11101: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11102: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11103: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11106: {

                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 物品编号
                view.setBigUint64(offset, data.itemNo, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 数量
                view.setUint16(offset, data.number, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 类型
                view.setUint8(offset, data.type, false);
                offset = offset + 1;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11101: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 物品编号
                    const dataDataItemNo = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 物品ID
                    const dataDataItemId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 数量
                    const dataDataNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"itemNo": dataDataItemNo, "itemId": dataDataItemId, "type": dataDataType, "number": dataDataNumber};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11101, "data": data};
            }
            case 11102: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 物品编号
                    const dataDataItemNo = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 物品ID
                    const dataDataItemId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 数量
                    const dataDataNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"itemNo": dataDataItemNo, "itemId": dataDataItemId, "type": dataDataType, "number": dataDataNumber};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11102, "data": data};
            }
            case 11103: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 物品编号
                    const dataDataItemNo = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 物品ID
                    const dataDataItemId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 数量
                    const dataDataNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"itemNo": dataDataItemNo, "itemId": dataDataItemId, "type": dataDataType, "number": dataDataNumber};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11103, "data": data};
            }
            case 11104: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 物品编号
                    const dataDataItemNo = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 物品ID
                    const dataDataItemId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataData = {"itemNo": dataDataItemNo, "itemId": dataDataItemId, "type": dataDataType};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11104, "data": data};
            }
            case 11106: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11106, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}