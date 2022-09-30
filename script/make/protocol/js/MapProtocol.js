export function encodeMapProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 20012: {
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // x坐标
            view.setUint16(offset, data["x"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // y坐标
            view.setUint16(offset, data["y"], false);
            offset = offset + 2;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 20014: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 技能Id
            view.setUint32(offset, data["skillId"], false);
            offset = offset + 4;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 对象列表
            const targetListData = data["targetList"];
            view.setUint16(offset, targetListData.length, false);
            offset = offset + 2;
            for (const targetListDataItem of targetListData) {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // ID
                view.setBigUint64(offset, targetListDataItem["targetId"], false);
                offset = offset + 8;
            }
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeMapProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 20011: {
            // 对象列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // ID
                const id = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 类型
                const type = view.getUint8(offset, false);
                offset = offset + 1;
                // 战力
                const fc = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 血量
                const hp = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 健康
                const health = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 技能列表
                const skill = [];
                let skillLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--skillLength >= 0) {
                    // 技能ID
                    const skillId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const number = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    skill.push({skillId, time, number});
                }
                // Buff列表
                const buff = [];
                let buffLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--buffLength >= 0) {
                    // BuffID
                    const buffId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const overlap = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    buff.push({buffId, expireTime, overlap});
                }
                // X坐标
                const x = view.getUint16(offset, false);
                offset = offset + 2;
                // Y坐标
                const y = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({id, type, fc, hp, health, skill, buff, x, y});
            }
            return {list};
        }
        case 20012: {
            // ID
            const id = view.getBigUint64(offset, false);
            offset = offset + 8;
            // X坐标
            const x = view.getUint16(offset, false);
            offset = offset + 2;
            // Y坐标
            const y = view.getUint16(offset, false);
            offset = offset + 2;
            return {id, x, y};
        }
        case 20013: {
            // ID
            const id = view.getBigUint64(offset, false);
            offset = offset + 8;
            return {id};
        }
        case 20014: {
            // 战斗对象Id
            const fighterId = view.getBigUint64(offset, false);
            offset = offset + 8;
            // 技能Id
            const performSkillId = view.getUint32(offset, false);
            offset = offset + 4;
            // 对象列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // ID
                const id = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 类型
                const type = view.getUint8(offset, false);
                offset = offset + 1;
                // 战力
                const fc = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 血量
                const hp = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 健康
                const health = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 技能列表
                const skill = [];
                let skillLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--skillLength >= 0) {
                    // 技能ID
                    const skillId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const number = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    skill.push({skillId, time, number});
                }
                // Buff列表
                const buff = [];
                let buffLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--buffLength >= 0) {
                    // BuffID
                    const buffId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const overlap = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    buff.push({buffId, expireTime, overlap});
                }
                // X坐标
                const x = view.getUint16(offset, false);
                offset = offset + 2;
                // Y坐标
                const y = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({id, type, fc, hp, health, skill, buff, x, y});
            }
            return {fighterId, performSkillId, list};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}