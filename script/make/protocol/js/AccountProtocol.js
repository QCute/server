export class AccountHeartbeatRequest {
    /** @type {number} protocol **/
    protocol = 10000;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class AccountHeartbeatResponse {
    /** @type {number} protocol **/
    protocol = 10000;
    /**
     * @type {string} data
    **/
    data;
}

export class AccountQueryRequest {
    /** @type {number} protocol **/
    protocol = 10001;
    /**
     * @type {{
     *     serverId: number;                                                                        // 服务器ID
     *     accountName: string;                                                                     // 账户名
     * }} data
    **/
    data;
}

export class AccountQueryResponse {
    /** @type {number} protocol **/
    protocol = 10001;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     list: Array<{
     *         roleId: BigInt;                                                                      // 角色ID
     *         roleName: string;                                                                    // 角色名
     *     }>;                                                                                      // 角色名列表
     * }} data
    **/
    data;
}

export class AccountCreateRequest {
    /** @type {number} protocol **/
    protocol = 10002;
    /**
     * @type {{
     *     roleName: string;                                                                        // 角色名
     *     serverId: number;                                                                        // 服务器ID
     *     accountName: string;                                                                     // 账户名
     *     sex: number;                                                                             // 性别
     *     classes: number;                                                                         // 职业
     *     channel: string;                                                                         // 渠道
     *     deviceId: string;                                                                        // 设备
     *     mac: string;                                                                             // mac地址
     *     deviceType: string;                                                                      // 设备类型
     * }} data
    **/
    data;
}

export class AccountCreateResponse {
    /** @type {number} protocol **/
    protocol = 10002;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     roleId: BigInt;                                                                          // 角色ID
     *     roleName: string;                                                                        // 角色名
     * }} data
    **/
    data;
}

export class AccountLoginRequest {
    /** @type {number} protocol **/
    protocol = 10003;
    /**
     * @type {{
     *     roleId: BigInt;                                                                          // 角色ID
     *     roleName: string;                                                                        // 角色名
     *     serverId: number;                                                                        // 服务器ID
     *     accountName: string;                                                                     // 账户名
     * }} data
    **/
    data;
}

export class AccountLoginResponse {
    /** @type {number} protocol **/
    protocol = 10003;
    /**
     * @type {string} data
    **/
    data;
}

export class AccountLogoutRequest {
    /** @type {number} protocol **/
    protocol = 10004;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class AccountLogoutResponse {
    /** @type {number} protocol **/
    protocol = 10004;
    /**
     * @type {string} data
    **/
    data;
}

export default class AccountProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 10000: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 10001: {

                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 服务器ID
                view.setUint16(offset, data.serverId, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 账户名
                const dataAccountNameArray = textEncoder.encode(data.accountName);
                view.setUint16(offset, dataAccountNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataAccountNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataAccountNameArray);
                offset = offset + dataAccountNameArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 10002: {

                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色名
                const dataRoleNameArray = textEncoder.encode(data.roleName);
                view.setUint16(offset, dataRoleNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataRoleNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataRoleNameArray);
                offset = offset + dataRoleNameArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 服务器ID
                view.setUint16(offset, data.serverId, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 账户名
                const dataAccountNameArray = textEncoder.encode(data.accountName);
                view.setUint16(offset, dataAccountNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataAccountNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataAccountNameArray);
                offset = offset + dataAccountNameArray.length;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 性别
                view.setUint8(offset, data.sex, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 职业
                view.setUint8(offset, data.classes, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 渠道
                const dataChannelArray = textEncoder.encode(data.channel);
                view.setUint16(offset, dataChannelArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataChannelArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataChannelArray);
                offset = offset + dataChannelArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 设备
                const dataDeviceIdArray = textEncoder.encode(data.deviceId);
                view.setUint16(offset, dataDeviceIdArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataDeviceIdArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataDeviceIdArray);
                offset = offset + dataDeviceIdArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // mac地址
                const dataMacArray = textEncoder.encode(data.mac);
                view.setUint16(offset, dataMacArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataMacArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataMacArray);
                offset = offset + dataMacArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 设备类型
                const dataDeviceTypeArray = textEncoder.encode(data.deviceType);
                view.setUint16(offset, dataDeviceTypeArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataDeviceTypeArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataDeviceTypeArray);
                offset = offset + dataDeviceTypeArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 10003: {

                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色ID
                view.setBigUint64(offset, data.roleId, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色名
                const dataRoleNameArray = textEncoder.encode(data.roleName);
                view.setUint16(offset, dataRoleNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataRoleNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataRoleNameArray);
                offset = offset + dataRoleNameArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 服务器ID
                view.setUint16(offset, data.serverId, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 账户名
                const dataAccountNameArray = textEncoder.encode(data.accountName);
                view.setUint16(offset, dataAccountNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataAccountNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataAccountNameArray);
                offset = offset + dataAccountNameArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 10004: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 10000: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 10000, "data": data};
            }
            case 10001: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 角色名列表
                const dataList = [];
                let dataListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataListLength >= 0) {
                    // 
                    // 角色ID
                    const dataListDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名
                    const dataListDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataListDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataListDataRoleNameLength));
                    const dataListDataRoleName = textDecoder.decode(dataListDataRoleNameArray);
                    offset = offset + dataListDataRoleNameLength;
                    // object
                    const dataListData = {"roleId": dataListDataRoleId, "roleName": dataListDataRoleName};
                    // add
                    dataList.push(dataListData);
                }
                // object
                const data = {"result": dataResult, "list": dataList};
                return {"protocol": 10001, "data": data};
            }
            case 10002: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 角色ID
                const dataRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名
                const dataRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataRoleNameLength));
                const dataRoleName = textDecoder.decode(dataRoleNameArray);
                offset = offset + dataRoleNameLength;
                // object
                const data = {"result": dataResult, "roleId": dataRoleId, "roleName": dataRoleName};
                return {"protocol": 10002, "data": data};
            }
            case 10003: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 10003, "data": data};
            }
            case 10004: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 10004, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}