export function encodeGuildProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
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
            view.setBigUint64(offset, data["guildId"], false);
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
            view.setBigUint64(offset, data["guildId"], false);
            offset = offset + 8;
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
            view.setBigUint64(offset, data["roleId"], false);
            offset = offset + 8;
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
            view.setBigUint64(offset, data["roleId"], false);
            offset = offset + 8;
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
            view.setBigUint64(offset, data["roleId"], false);
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
        case 30120: {
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 公告
            const noticeArray = textEncoder.encode(data["notice"]);
            view.setUint16(offset, noticeArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + noticeArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(noticeArray);
            offset = offset + noticeArray.length;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeGuildProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 30101: {
            // 公会列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
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
                // add
                list.push({guildId, guildName, createTime, leaderRoleId, leaderName});
            }
            return {list};
        }
        case 30102: {
            // 成员列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
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
                // add
                list.push({roleId, job, joinTime, roleName, sex, classes, vipLevel});
            }
            return {list};
        }
        case 30103: {
            // 申请列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
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
                // add
                list.push({roleId, applyTime, roleName, sex, classes, vipLevel});
            }
            return {list};
        }
        case 30104: {
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
            return {guildId, guildName, exp, wealth, level, createTime, notice, leaderRoleId, leaderName};
        }
        case 30105: {
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
            return {roleId, job, joinTime, roleName, sex, classes, vipLevel};
        }
        case 30106: {
            // 
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
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
                // add
                list.push({guildId, applyTime, guildName});
            }
            return {list};
        }
        case 30107: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30108: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30109: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30110: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30111: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30112: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30113: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30114: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30115: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30116: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30117: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30118: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30119: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 30120: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}