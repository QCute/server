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
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
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
                    // add
                    list.push({type, order, key, value, time, name, serverId});
                }
                return {list};
            }
            case 19202: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
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
                    // 等级
                    const level = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // add
                    list.push({type, order, key, value, time, name, serverId, level, classes});
                }
                return {list};
            }
            case 19203: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
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
                    // 等级
                    const level = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const sex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // add
                    list.push({type, order, key, value, time, name, serverId, level, classes, sex});
                }
                return {list};
            }
            case 19204: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
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
                    // 等级
                    const level = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const sex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // VIP等级
                    const vipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // add
                    list.push({type, order, key, value, time, name, serverId, level, classes, sex, vipLevel});
                }
                return {list};
            }
            case 19205: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
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
                    // 等级
                    const level = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const sex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // VIP等级
                    const vipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 头像
                    const avatar = view.getUint8(offset, false);
                    offset = offset + 1;
                    // add
                    list.push({type, order, key, value, time, name, serverId, level, classes, sex, vipLevel, avatar});
                }
                return {list};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}