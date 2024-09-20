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
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Rank
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
                    const rank = {type, order, key, value, time, name, serverId};
                    // add
                    list.push(rank);
                }
                return {list};
            }
            case 19102: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Rank
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
                    const level = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const other = {level, classes};
                    // object
                    const rank = {type, order, key, value, time, name, serverId, other};
                    // add
                    list.push(rank);
                }
                return {list};
            }
            case 19103: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Rank
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
                    const level = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 性别
                    const sex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const other = {level, classes, sex};
                    // object
                    const rank = {type, order, key, value, time, name, serverId, other};
                    // add
                    list.push(rank);
                }
                return {list};
            }
            case 19104: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Rank
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
                    // object
                    const other = {level, classes, sex, vipLevel};
                    // object
                    const rank = {type, order, key, value, time, name, serverId, other};
                    // add
                    list.push(rank);
                }
                return {list};
            }
            case 19105: {
                // 排行榜
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Rank
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
                    // object
                    const other = {level, classes, sex, vipLevel, avatar};
                    // object
                    const rank = {type, order, key, value, time, name, serverId, other};
                    // add
                    list.push(rank);
                }
                return {list};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}