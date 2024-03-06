export default class GuildProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 30101: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30102: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30103: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30104: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30105: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30106: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30107: {
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 类型
                view.setUint8(offset, data["type"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 公会名
                const guildNameArray = textEncoder.encode(data["guildName"]);
                view.setUint16(offset, guildNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + guildNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(guildNameArray);
                offset = offset + guildNameArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30108: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 公会ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30109: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 公会ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30110: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30111: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30112: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30113: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30114: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30115: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30116: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30117: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30118: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 角色ID
                view.setBigUint64(offset, data["roleId"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 位置
                view.setUint8(offset, data["job"], false);
                offset = offset + 1;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30119: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 30120: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 公告
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
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 30101: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 公会ID
                    const guildId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 公会名字
                    const guildNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const guildNameArray = new Uint8Array(view.buffer.slice(offset, offset + guildNameLength));
                    const guildName = textDecoder.decode(guildNameArray);
                    offset = offset + guildNameLength;
                    // 创建时间
                    const createTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 会长角色ID
                    const leaderRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 会长名字
                    const leaderNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const leaderNameArray = new Uint8Array(view.buffer.slice(offset, offset + leaderNameLength));
                    const leaderName = textDecoder.decode(leaderNameArray);
                    offset = offset + leaderNameLength;
                    // object
                    const guild = {"guildId": guildId, "guildName": guildName, "createTime": createTime, "leaderRoleId": leaderRoleId, "leaderName": leaderName};
                    // add
                    data.push(guild);
                }
                return data;
            }
            case 30102: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 成员ID
                    const roleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 职位
                    const job = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 加入时间
                    const joinTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 成员名字
                    const roleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                    const roleName = textDecoder.decode(roleNameArray);
                    offset = offset + roleNameLength;
                    // 性别
                    const sex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // Vip等级
                    const vipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const guildRole = {"roleId": roleId, "job": job, "joinTime": joinTime, "roleName": roleName, "sex": sex, "classes": classes, "vipLevel": vipLevel};
                    // add
                    data.push(guildRole);
                }
                return data;
            }
            case 30103: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 申请ID
                    const roleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 申请时间
                    const applyTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 申请名字
                    const roleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                    const roleName = textDecoder.decode(roleNameArray);
                    offset = offset + roleNameLength;
                    // 性别
                    const sex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 职业
                    const classes = view.getUint8(offset, false);
                    offset = offset + 1;
                    // Vip等级
                    const vipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const guildApply = {"roleId": roleId, "applyTime": applyTime, "roleName": roleName, "sex": sex, "classes": classes, "vipLevel": vipLevel};
                    // add
                    data.push(guildApply);
                }
                return data;
            }
            case 30104: {
                // 
                // 公会ID
                const guildId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 公会名字
                const guildNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const guildNameArray = new Uint8Array(view.buffer.slice(offset, offset + guildNameLength));
                const guildName = textDecoder.decode(guildNameArray);
                offset = offset + guildNameLength;
                // 经验
                const exp = view.getUint32(offset, false);
                offset = offset + 4;
                // 财富
                const wealth = view.getUint32(offset, false);
                offset = offset + 4;
                // 等级
                const level = view.getUint8(offset, false);
                offset = offset + 1;
                // 创建时间
                const createTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 公告
                const noticeLength = view.getUint16(offset, false);
                offset = offset + 2;
                const noticeArray = new Uint8Array(view.buffer.slice(offset, offset + noticeLength));
                const notice = textDecoder.decode(noticeArray);
                offset = offset + noticeLength;
                // 会长角色ID
                const leaderRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 会长名字
                const leaderNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const leaderNameArray = new Uint8Array(view.buffer.slice(offset, offset + leaderNameLength));
                const leaderName = textDecoder.decode(leaderNameArray);
                offset = offset + leaderNameLength;
                // object
                const guild = {"guildId": guildId, "guildName": guildName, "exp": exp, "wealth": wealth, "level": level, "createTime": createTime, "notice": notice, "leaderRoleId": leaderRoleId, "leaderName": leaderName};
                return guild;
            }
            case 30105: {
                // 
                // 成员ID
                const roleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 职位
                const job = view.getUint8(offset, false);
                offset = offset + 1;
                // 加入时间
                const joinTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 成员名字
                const roleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                const roleName = textDecoder.decode(roleNameArray);
                offset = offset + roleNameLength;
                // 性别
                const sex = view.getUint8(offset, false);
                offset = offset + 1;
                // 职业
                const classes = view.getUint8(offset, false);
                offset = offset + 1;
                // Vip等级
                const vipLevel = view.getUint8(offset, false);
                offset = offset + 1;
                // object
                const guildRole = {"roleId": roleId, "job": job, "joinTime": joinTime, "roleName": roleName, "sex": sex, "classes": classes, "vipLevel": vipLevel};
                return guildRole;
            }
            case 30106: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 公会ID
                    const guildId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 申请时间
                    const applyTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 公会名字
                    const guildNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const guildNameArray = new Uint8Array(view.buffer.slice(offset, offset + guildNameLength));
                    const guildName = textDecoder.decode(guildNameArray);
                    offset = offset + guildNameLength;
                    // object
                    const guildApply = {"guildId": guildId, "applyTime": applyTime, "guildName": guildName};
                    // add
                    data.push(guildApply);
                }
                return data;
            }
            case 30107: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30108: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30109: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30110: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30111: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30112: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30113: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30114: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30115: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30116: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30117: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30118: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30119: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 30120: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}