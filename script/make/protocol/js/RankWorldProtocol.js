export class RankWorldLevelRequest {
    /** @type {number} protocol **/
    protocol = 19201;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankWorldLevelResponse {
    /** @type {number} protocol **/
    protocol = 19201;
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

export class RankWorldFightRequest {
    /** @type {number} protocol **/
    protocol = 19202;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankWorldFightResponse {
    /** @type {number} protocol **/
    protocol = 19202;
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

export class RankWorldAchievementRequest {
    /** @type {number} protocol **/
    protocol = 19203;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankWorldAchievementResponse {
    /** @type {number} protocol **/
    protocol = 19203;
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

export class RankWorldWealthRequest {
    /** @type {number} protocol **/
    protocol = 19204;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankWorldWealthResponse {
    /** @type {number} protocol **/
    protocol = 19204;
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

export class RankWorldClassesRequest {
    /** @type {number} protocol **/
    protocol = 19205;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class RankWorldClassesResponse {
    /** @type {number} protocol **/
    protocol = 19205;
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

export default class RankWorldProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 19201: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19202: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19203: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19204: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 19205: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 19201: {
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
                return {"protocol": 19201, "data": data};
            }
            case 19202: {
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
                return {"protocol": 19202, "data": data};
            }
            case 19203: {
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
                return {"protocol": 19203, "data": data};
            }
            case 19204: {
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
                return {"protocol": 19204, "data": data};
            }
            case 19205: {
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
                return {"protocol": 19205, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}