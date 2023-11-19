export class FriendQueryRequest {
    /** @type {number} protocol **/
    protocol = 11501;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class FriendQueryResponse {
    /** @type {number} protocol **/
    protocol = 11501;
    /**
     * @type {Array<{
     *     friendRoleId: BigInt;                                                                    // 好友角色ID
     *     friendName: string;                                                                      // 好友名字
     *     relation: number;                                                                        // 关系状态(申请:1/好友:2/黑名单:3)
     *     time: number;                                                                            // 添加/修改状态时间
     * }>} data
    **/
    data;
}

export class FriendApplyRequest {
    /** @type {number} protocol **/
    protocol = 11502;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class FriendApplyResponse {
    /** @type {number} protocol **/
    protocol = 11502;
    /**
     * @type {string} data
    **/
    data;
}

export class FriendAgreeRequest {
    /** @type {number} protocol **/
    protocol = 11503;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class FriendAgreeResponse {
    /** @type {number} protocol **/
    protocol = 11503;
    /**
     * @type {string} data
    **/
    data;
}

export class FriendDeleteRequest {
    /** @type {number} protocol **/
    protocol = 11504;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class FriendDeleteResponse {
    /** @type {number} protocol **/
    protocol = 11504;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     friendRoleId: BigInt;                                                                    // 好友角色ID
     * }} data
    **/
    data;
}

export class FriendBlockRequest {
    /** @type {number} protocol **/
    protocol = 11505;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class FriendBlockResponse {
    /** @type {number} protocol **/
    protocol = 11505;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     friendRoleId: BigInt;                                                                    // 好友角色ID
     * }} data
    **/
    data;
}

export class FriendCancelBlockRequest {
    /** @type {number} protocol **/
    protocol = 11506;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class FriendCancelBlockResponse {
    /** @type {number} protocol **/
    protocol = 11506;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     friendRoleId: BigInt;                                                                    // 好友角色ID
     * }} data
    **/
    data;
}

export default class FriendProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11501: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11502: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11503: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11504: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11505: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11506: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11501: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 好友角色ID
                    const dataDataFriendRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 好友名字
                    const dataDataFriendNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataFriendNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataFriendNameLength));
                    const dataDataFriendName = textDecoder.decode(dataDataFriendNameArray);
                    offset = offset + dataDataFriendNameLength;
                    // 关系状态(申请:1/好友:2/黑名单:3)
                    const dataDataRelation = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 添加/修改状态时间
                    const dataDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"friendRoleId": dataDataFriendRoleId, "friendName": dataDataFriendName, "relation": dataDataRelation, "time": dataDataTime};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11501, "data": data};
            }
            case 11502: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11502, "data": data};
            }
            case 11503: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11503, "data": data};
            }
            case 11504: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 好友角色ID
                const dataFriendRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": dataResult, "friendRoleId": dataFriendRoleId};
                return {"protocol": 11504, "data": data};
            }
            case 11505: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 好友角色ID
                const dataFriendRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": dataResult, "friendRoleId": dataFriendRoleId};
                return {"protocol": 11505, "data": data};
            }
            case 11506: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 好友角色ID
                const dataFriendRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": dataResult, "friendRoleId": dataFriendRoleId};
                return {"protocol": 11506, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}