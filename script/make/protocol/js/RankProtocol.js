export class RankLevelRequest {
    /** @type {number} protocol **/
    protocol = 19001;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankLevelResponse {
    /** @type {number} protocol **/
    protocol = 19001;
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

export class RankFightRequest {
    /** @type {number} protocol **/
    protocol = 19002;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankFightResponse {
    /** @type {number} protocol **/
    protocol = 19002;
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

export class RankAchievementRequest {
    /** @type {number} protocol **/
    protocol = 19003;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankAchievementResponse {
    /** @type {number} protocol **/
    protocol = 19003;
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

export class RankWealthRequest {
    /** @type {number} protocol **/
    protocol = 19004;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankWealthResponse {
    /** @type {number} protocol **/
    protocol = 19004;
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

export class RankClassesRequest {
    /** @type {number} protocol **/
    protocol = 19005;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankClassesResponse {
    /** @type {number} protocol **/
    protocol = 19005;
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

export default class RankProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 19001: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19002: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19003: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19004: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19005: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 19001: {
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
                return {"protocol": 19001, "data": data};
            }
            case 19002: {
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
                return {"protocol": 19002, "data": data};
            }
            case 19003: {
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
                return {"protocol": 19003, "data": data};
            }
            case 19004: {
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
                return {"protocol": 19004, "data": data};
            }
            case 19005: {
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
                return {"protocol": 19005, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}