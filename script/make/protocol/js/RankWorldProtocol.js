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
                    const type = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const order = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const key = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const value = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const nameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const nameArray = new Uint8Array(view.buffer.slice(offset, offset + nameLength));
                    const name = textDecoder.decode(nameArray);
                    offset = offset + nameLength;
                    // 服务器ID
                    const serverId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const rank = {"type": type, "order": order, "key": key, "value": value, "time": time, "name": name, "serverId": serverId};
                    // add
                    data.push(rank);
                }
                return data;
            }
            case 19202: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const type = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const order = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const key = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const value = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const nameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const nameArray = new Uint8Array(view.buffer.slice(offset, offset + nameLength));
                    const name = textDecoder.decode(nameArray);
                    offset = offset + nameLength;
                    // 服务器ID
                    const serverId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const otherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const otherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const other = {"level": otherLevel, "classes": otherClasses};
                    // object
                    const rank = {"type": type, "order": order, "key": key, "value": value, "time": time, "name": name, "serverId": serverId, "other": other};
                    // add
                    data.push(rank);
                }
                return data;
            }
            case 19203: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const type = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const order = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const key = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const value = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const nameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const nameArray = new Uint8Array(view.buffer.slice(offset, offset + nameLength));
                    const name = textDecoder.decode(nameArray);
                    offset = offset + nameLength;
                    // 服务器ID
                    const serverId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const otherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const otherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const otherSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const other = {"level": otherLevel, "classes": otherClasses, "sex": otherSex};
                    // object
                    const rank = {"type": type, "order": order, "key": key, "value": value, "time": time, "name": name, "serverId": serverId, "other": other};
                    // add
                    data.push(rank);
                }
                return data;
            }
            case 19204: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const type = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const order = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const key = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const value = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const nameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const nameArray = new Uint8Array(view.buffer.slice(offset, offset + nameLength));
                    const name = textDecoder.decode(nameArray);
                    offset = offset + nameLength;
                    // 服务器ID
                    const serverId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const otherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const otherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const otherSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // VIP等级
                    const otherVipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const other = {"level": otherLevel, "classes": otherClasses, "sex": otherSex, "vipLevel": otherVipLevel};
                    // object
                    const rank = {"type": type, "order": order, "key": key, "value": value, "time": time, "name": name, "serverId": serverId, "other": other};
                    // add
                    data.push(rank);
                }
                return data;
            }
            case 19205: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 类型
                    const type = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 排名
                    const order = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 键
                    const key = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 值
                    const value = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 名字
                    const nameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const nameArray = new Uint8Array(view.buffer.slice(offset, offset + nameLength));
                    const name = textDecoder.decode(nameArray);
                    offset = offset + nameLength;
                    // 服务器ID
                    const serverId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 
                    // 等级
                    const otherLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const otherClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const otherSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // VIP等级
                    const otherVipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 头像
                    const otherAvatar = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const other = {"level": otherLevel, "classes": otherClasses, "sex": otherSex, "vipLevel": otherVipLevel, "avatar": otherAvatar};
                    // object
                    const rank = {"type": type, "order": order, "key": key, "value": value, "time": time, "name": name, "serverId": serverId, "other": other};
                    // add
                    data.push(rank);
                }
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}