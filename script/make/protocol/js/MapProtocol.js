export default class MapProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 20001: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 20012: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // X坐标
                view.setUint16(offset, data["data"]["x"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // Y坐标
                view.setUint16(offset, data["data"]["y"], false);
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
                view.setUint32(offset, data["data"]["skillId"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 战斗对象ID列表
                const targetListData = data["data"]["targetList"];
                view.setUint16(offset, targetListData.length, false);
                offset = offset + 2;
                for (const targetListDataItem of targetListData) {
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 战斗对象ID
                    view.setBigUint64(offset, targetListDataItem, false);
                    offset = offset + 8;
                }
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 20001: {
                // 

                // object
                const data = {};
                return data;
            }
            case 20011: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const id = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const fc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const hp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const health = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const attribute = {fc, hp, health};
                    // 技能列表
                    const skill = [];
                    let skillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--skillLength >= 0) {
                        // 
                        // 技能ID
                        const skillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const time = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const number = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const battleSkill = {skillId, time, number};
                        // add
                        skill.push(battleSkill);
                    }
                    // Buff列表
                    const buff = [];
                    let buffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--buffLength >= 0) {
                        // 
                        // BuffID
                        const buffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const expireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const overlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const battleBuff = {buffId, expireTime, overlap};
                        // add
                        buff.push(battleBuff);
                    }
                    // X坐标
                    const x = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const y = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const fighter = {id, type, attribute, skill, buff, x, y};
                    // add
                    data.push(fighter);
                }
                return data;
            }
            case 20012: {
                // 
                // ID
                const id = view.getBigUint64(offset, false);
                offset = offset + 8;
                // X坐标
                const x = view.getUint16(offset, false);
                offset = offset + 2;
                // Y坐标
                const y = view.getUint16(offset, false);
                offset = offset + 2;
                // object
                const fighter = {id, x, y};
                return fighter;
            }
            case 20013: {
                // 
                // 战斗对象ID
                const id = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const fighter = {id};
                return fighter;
            }
            case 20014: {
                // 
                // 战斗对象Id
                const fighterId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 技能Id
                const performSkillId = view.getUint32(offset, false);
                offset = offset + 4;
                // 
                const fighterList = [];
                let fighterListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--fighterListLength >= 0) {
                    // 
                    // ID
                    const id = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const fc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const hp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const health = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const attribute = {fc, hp, health};
                    // 技能列表
                    const skill = [];
                    let skillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--skillLength >= 0) {
                        // 
                        // 技能ID
                        const skillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const time = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const number = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const battleSkill = {skillId, time, number};
                        // add
                        skill.push(battleSkill);
                    }
                    // Buff列表
                    const buff = [];
                    let buffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--buffLength >= 0) {
                        // 
                        // BuffID
                        const buffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const expireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const overlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const battleBuff = {buffId, expireTime, overlap};
                        // add
                        buff.push(battleBuff);
                    }
                    // X坐标
                    const x = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const y = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const fighter = {id, type, attribute, skill, buff, x, y};
                    // add
                    fighterList.push(fighter);
                }
                // object
                const data = {fighterId, performSkillId, fighterList};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}