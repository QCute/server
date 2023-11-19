export class RankCenterLevelRequest {
    /** @type {number} protocol **/
    protocol = 19101;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankCenterLevelResponse {
    /** @type {number} protocol **/
    protocol = 19101;
    /**
     * @type {Array<{
     *     type: number;                                                                            // 类型
     *     order: BigInt;                                                                           // 排名
     *     key: BigInt;                                                                             // 键
     *     value: BigInt;                                                                           // 值
     *     time: number;                                                                            // 时间
     *     name: string;                                                                            // 名字
     *     serverId: number;                                                                        // 服务器ID
     * }>} data
    **/
    data;
}

export class RankCenterFightRequest {
    /** @type {number} protocol **/
    protocol = 19102;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankCenterFightResponse {
    /** @type {number} protocol **/
    protocol = 19102;
    /**
     * @type {Array<{
     *     type: number;                                                                            // 类型
     *     order: BigInt;                                                                           // 排名
     *     key: BigInt;                                                                             // 键
     *     value: BigInt;                                                                           // 值
     *     time: number;                                                                            // 时间
     *     name: string;                                                                            // 名字
     *     serverId: number;                                                                        // 服务器ID
     *     other: {
     *         level: number;                                                                       // 等级
     *         classes: number;                                                                     // 职业
     *     };                                                                                       // 
     * }>} data
    **/
    data;
}

export class RankCenterAchievementRequest {
    /** @type {number} protocol **/
    protocol = 19103;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankCenterAchievementResponse {
    /** @type {number} protocol **/
    protocol = 19103;
    /**
     * @type {Array<{
     *     type: number;                                                                            // 类型
     *     order: BigInt;                                                                           // 排名
     *     key: BigInt;                                                                             // 键
     *     value: BigInt;                                                                           // 值
     *     time: number;                                                                            // 时间
     *     name: string;                                                                            // 名字
     *     serverId: number;                                                                        // 服务器ID
     *     other: {
     *         level: number;                                                                       // 等级
     *         classes: number;                                                                     // 职业
     *         sex: number;                                                                         // 性别
     *     };                                                                                       // 
     * }>} data
    **/
    data;
}

export class RankCenterWealthRequest {
    /** @type {number} protocol **/
    protocol = 19104;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankCenterWealthResponse {
    /** @type {number} protocol **/
    protocol = 19104;
    /**
     * @type {Array<{
     *     type: number;                                                                            // 类型
     *     order: BigInt;                                                                           // 排名
     *     key: BigInt;                                                                             // 键
     *     value: BigInt;                                                                           // 值
     *     time: number;                                                                            // 时间
     *     name: string;                                                                            // 名字
     *     serverId: number;                                                                        // 服务器ID
     *     other: {
     *         level: number;                                                                       // 等级
     *         classes: number;                                                                     // 职业
     *         sex: number;                                                                         // 性别
     *         vipLevel: number;                                                                    // VIP等级
     *     };                                                                                       // 
     * }>} data
    **/
    data;
}

export class RankCenterClassesRequest {
    /** @type {number} protocol **/
    protocol = 19105;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankCenterClassesResponse {
    /** @type {number} protocol **/
    protocol = 19105;
    /**
     * @type {Array<{
     *     type: number;                                                                            // 类型
     *     order: BigInt;                                                                           // 排名
     *     key: BigInt;                                                                             // 键
     *     value: BigInt;                                                                           // 值
     *     time: number;                                                                            // 时间
     *     name: string;                                                                            // 名字
     *     serverId: number;                                                                        // 服务器ID
     *     other: {
     *         level: number;                                                                       // 等级
     *         classes: number;                                                                     // 职业
     *         sex: number;                                                                         // 性别
     *         vipLevel: number;                                                                    // VIP等级
     *         avatar: number;                                                                      // 头像
     *     };                                                                                       // 
     * }>} data
    **/
    data;
}

export default class RankCenterProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 19101: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19102: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19103: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19104: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19105: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 19101: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const dataDataType = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const dataDataOrder = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const dataDataKey = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const dataDataValue = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const dataDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const dataDataNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataNameLength));
                    const dataDataName = textDecoder.decode(dataDataNameArray);
                    offset = offset + dataDataNameLength;
                    // 服务器ID
                    const dataDataServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"type": dataDataType, "order": dataDataOrder, "key": dataDataKey, "value": dataDataValue, "time": dataDataTime, "name": dataDataName, "serverId": dataDataServerId};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 19101, "data": data};
            }
            case 19102: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const dataDataType = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const dataDataOrder = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const dataDataKey = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const dataDataValue = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const dataDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const dataDataNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataNameLength));
                    const dataDataName = textDecoder.decode(dataDataNameArray);
                    offset = offset + dataDataNameLength;
                    // 服务器ID
                    const dataDataServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const dataDataOtherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const dataDataOtherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataDataOther = {"level": dataDataOtherLevel, "classes": dataDataOtherClasses};
                    // object
                    const dataData = {"type": dataDataType, "order": dataDataOrder, "key": dataDataKey, "value": dataDataValue, "time": dataDataTime, "name": dataDataName, "serverId": dataDataServerId, "other": dataDataOther};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 19102, "data": data};
            }
            case 19103: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const dataDataType = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const dataDataOrder = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const dataDataKey = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const dataDataValue = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const dataDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const dataDataNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataNameLength));
                    const dataDataName = textDecoder.decode(dataDataNameArray);
                    offset = offset + dataDataNameLength;
                    // 服务器ID
                    const dataDataServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const dataDataOtherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const dataDataOtherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const dataDataOtherSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataDataOther = {"level": dataDataOtherLevel, "classes": dataDataOtherClasses, "sex": dataDataOtherSex};
                    // object
                    const dataData = {"type": dataDataType, "order": dataDataOrder, "key": dataDataKey, "value": dataDataValue, "time": dataDataTime, "name": dataDataName, "serverId": dataDataServerId, "other": dataDataOther};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 19103, "data": data};
            }
            case 19104: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const dataDataType = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const dataDataOrder = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const dataDataKey = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const dataDataValue = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const dataDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const dataDataNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataNameLength));
                    const dataDataName = textDecoder.decode(dataDataNameArray);
                    offset = offset + dataDataNameLength;
                    // 服务器ID
                    const dataDataServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const dataDataOtherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const dataDataOtherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const dataDataOtherSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // VIP等级
                    const dataDataOtherVipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataDataOther = {"level": dataDataOtherLevel, "classes": dataDataOtherClasses, "sex": dataDataOtherSex, "vipLevel": dataDataOtherVipLevel};
                    // object
                    const dataData = {"type": dataDataType, "order": dataDataOrder, "key": dataDataKey, "value": dataDataValue, "time": dataDataTime, "name": dataDataName, "serverId": dataDataServerId, "other": dataDataOther};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 19104, "data": data};
            }
            case 19105: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const dataDataType = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const dataDataOrder = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const dataDataKey = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const dataDataValue = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const dataDataTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const dataDataNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataNameLength));
                    const dataDataName = textDecoder.decode(dataDataNameArray);
                    offset = offset + dataDataNameLength;
                    // 服务器ID
                    const dataDataServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const dataDataOtherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const dataDataOtherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const dataDataOtherSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // VIP等级
                    const dataDataOtherVipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 头像
                    const dataDataOtherAvatar = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataDataOther = {"level": dataDataOtherLevel, "classes": dataDataOtherClasses, "sex": dataDataOtherSex, "vipLevel": dataDataOtherVipLevel, "avatar": dataDataOtherAvatar};
                    // object
                    const dataData = {"type": dataDataType, "order": dataDataOrder, "key": dataDataKey, "value": dataDataValue, "time": dataDataTime, "name": dataDataName, "serverId": dataDataServerId, "other": dataDataOther};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 19105, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}