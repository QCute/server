export class WelfareSignRequest {
    /** @type {number} protocol **/
    protocol = 15001;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class WelfareSignResponse {
    /** @type {number} protocol **/
    protocol = 15001;
    /**
     * @type {string} data
    **/
    data;
}

export class WelfareAwardRequest {
    /** @type {number} protocol **/
    protocol = 15002;
    /**
     * @type {string} data
    **/
    data;
}

export class WelfareAwardResponse {
    /** @type {number} protocol **/
    protocol = 15002;
    /**
     * @type {string} data
    **/
    data;
}

export class WelfareQueryLuckyMoneyRequest {
    /** @type {number} protocol **/
    protocol = 15003;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class WelfareQueryLuckyMoneyResponse {
    /** @type {number} protocol **/
    protocol = 15003;
    /**
     * @type {{
     *     luckyMoneyNo: BigInt;                                                                    // 红包编号
     *     totalGold: BigInt;                                                                       // 总金币
     *     totalNumber: number;                                                                     // 总数量
     *     receiveNumber: number;                                                                   // 已经领取人数
     *     receiveList: Array<{
     *         serverId: number;                                                                    // 服务器Id
     *         roleId: BigInt;                                                                      // 角色Id
     *         roleName: string;                                                                    // 角色名
     *         gold: BigInt;                                                                        // 金币
     *         time: number;                                                                        // 领取时间
     *     }>;                                                                                      // 领取列表
     *     time: number;                                                                            // 发送时间
     * }} data
    **/
    data;
}

export class WelfareReceiveLuckyMoneyRequest {
    /** @type {number} protocol **/
    protocol = 15004;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class WelfareReceiveLuckyMoneyResponse {
    /** @type {number} protocol **/
    protocol = 15004;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     gold: BigInt;                                                                            // 金币
     * }} data
    **/
    data;
}



export class WelfareLuckyMoneyComingResponse {
    /** @type {number} protocol **/
    protocol = 15005;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export default class WelfareProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 15001: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 15002: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 兑换码
                const dataArray = textEncoder.encode(data);
                view.setUint16(offset, dataArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataArray);
                offset = offset + dataArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 15003: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 红包编号
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 15004: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 红包编号
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 15001: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 15001, "data": data};
            }
            case 15002: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 15002, "data": data};
            }
            case 15003: {
                // 
                // 红包编号
                const dataLuckyMoneyNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 总金币
                const dataTotalGold = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 总数量
                const dataTotalNumber = view.getUint32(offset, false);
                offset = offset + 4;
                // 已经领取人数
                const dataReceiveNumber = view.getUint16(offset, false);
                offset = offset + 2;
                // 领取列表
                const dataReceiveList = [];
                let dataReceiveListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataReceiveListLength >= 0) {
                    // 
                    // 服务器Id
                    const dataReceiveListDataServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 角色Id
                    const dataReceiveListDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名
                    const dataReceiveListDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataReceiveListDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataReceiveListDataRoleNameLength));
                    const dataReceiveListDataRoleName = textDecoder.decode(dataReceiveListDataRoleNameArray);
                    offset = offset + dataReceiveListDataRoleNameLength;
                    // 金币
                    const dataReceiveListDataGold = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 领取时间
                    const dataReceiveListDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataReceiveListData = {"serverId": dataReceiveListDataServerId, "roleId": dataReceiveListDataRoleId, "roleName": dataReceiveListDataRoleName, "gold": dataReceiveListDataGold, "time": dataReceiveListDataTime};
                    // add
                    dataReceiveList.push(dataReceiveListData);
                }
                // 发送时间
                const dataTime = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const data = {"luckyMoneyNo": dataLuckyMoneyNo, "totalGold": dataTotalGold, "totalNumber": dataTotalNumber, "receiveNumber": dataReceiveNumber, "receiveList": dataReceiveList, "time": dataTime};
                return {"protocol": 15003, "data": data};
            }
            case 15004: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 金币
                const dataGold = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": dataResult, "gold": dataGold};
                return {"protocol": 15004, "data": data};
            }
            case 15005: {
                // 

                // object
                const data = {};
                return {"protocol": 15005, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}