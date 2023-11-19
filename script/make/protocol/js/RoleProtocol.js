export class RoleQueryRequest {
    /** @type {number} protocol **/
    protocol = 10101;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RoleQueryResponse {
    /** @type {number} protocol **/
    protocol = 10101;
    /**
     * @type {{
     *     roleId: BigInt;                                                                          // 角色ID
     *     roleName: string;                                                                        // 角色名
     *     sex: number;                                                                             // 性别
     *     classes: number;                                                                         // 职业
     *     level: BigInt;                                                                           // 等级
     * }} data
    **/
    data;
}

export class RoleAssetQueryRequest {
    /** @type {number} protocol **/
    protocol = 10102;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RoleAssetQueryResponse {
    /** @type {number} protocol **/
    protocol = 10102;
    /**
     * @type {{
     *     gold: BigInt;                                                                            // 金币
     *     silver: number;                                                                          // 银币
     *     copper: BigInt;                                                                          // 铜币
     *     exp: BigInt;                                                                             // 经验
     * }} data
    **/
    data;
}

export class RoleVipQueryRequest {
    /** @type {number} protocol **/
    protocol = 10103;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RoleVipQueryResponse {
    /** @type {number} protocol **/
    protocol = 10103;
    /**
     * @type {{
     *     vipLevel: number;                                                                        // 等级
     *     exp: BigInt;                                                                             // 经验
     *     expireTime: number;                                                                      // 过期时间
     * }} data
    **/
    data;
}

export default class RoleProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 10101: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 10102: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 10103: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 10101: {
                // 
                // 角色ID
                const dataRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名
                const dataRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataRoleNameLength));
                const dataRoleName = textDecoder.decode(dataRoleNameArray);
                offset = offset + dataRoleNameLength;
                // 性别
                const dataSex = view.getUint8(offset, false);
                offset = offset + 1;
                // 职业
                const dataClasses = view.getUint8(offset, false);
                offset = offset + 1;
                // 等级
                const dataLevel = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"roleId": dataRoleId, "roleName": dataRoleName, "sex": dataSex, "classes": dataClasses, "level": dataLevel};
                return {"protocol": 10101, "data": data};
            }
            case 10102: {
                // 
                // 金币
                const dataGold = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 银币
                const dataSilver = view.getUint32(offset, false);
                offset = offset + 4;
                // 铜币
                const dataCopper = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 经验
                const dataExp = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"gold": dataGold, "silver": dataSilver, "copper": dataCopper, "exp": dataExp};
                return {"protocol": 10102, "data": data};
            }
            case 10103: {
                // 
                // 等级
                const dataVipLevel = view.getUint8(offset, false);
                offset = offset + 1;
                // 经验
                const dataExp = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 过期时间
                const dataExpireTime = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const data = {"vipLevel": dataVipLevel, "exp": dataExp, "expireTime": dataExpireTime};
                return {"protocol": 10103, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}