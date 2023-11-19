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
                return data;
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
                return data;
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
                return data;
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
                return data;
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
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}