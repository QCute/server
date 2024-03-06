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
                view.setUint16(offset, data["x"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // Y坐标
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
                // 战斗对象ID列表
                const targetList = data["targetList"];
                view.setUint16(offset, targetList.length, false);
                offset = offset + 2;
                for (const targetListItem of targetList) {
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 战斗对象ID
                    view.setBigUint64(offset, targetListItem, false);
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
                // 地图编号
                const mapNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 地图ID
                const mapId = view.getUint32(offset, false);
                offset = offset + 4;
                // 
                const fighter = [];
                let fighterLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--fighterLength >= 0) {
                    // 
                    // ID
                    const fighterId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const fighterType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const fighterAttributeFc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const fighterAttributeHp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const fighterAttributeHealth = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const fighterAttribute = {"fc": fighterAttributeFc, "hp": fighterAttributeHp, "health": fighterAttributeHealth};
                    // 技能列表
                    const fighterSkill = [];
                    let fighterSkillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--fighterSkillLength >= 0) {
                        // 
                        // 技能ID
                        const fighterSkillSkillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const fighterSkillTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const fighterSkillNumber = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const fighterSkillBattleSkill = {"skillId": fighterSkillSkillId, "time": fighterSkillTime, "number": fighterSkillNumber};
                        // add
                        fighterSkill.push(fighterSkillBattleSkill);
                    }
                    // Buff列表
                    const fighterBuff = [];
                    let fighterBuffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--fighterBuffLength >= 0) {
                        // 
                        // BuffID
                        const fighterBuffBuffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const fighterBuffExpireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const fighterBuffOverlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const fighterBuffBattleBuff = {"buffId": fighterBuffBuffId, "expireTime": fighterBuffExpireTime, "overlap": fighterBuffOverlap};
                        // add
                        fighterBuff.push(fighterBuffBattleBuff);
                    }
                    // X坐标
                    const fighterX = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const fighterY = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const fighterFighter = {"id": fighterId, "type": fighterType, "attribute": fighterAttribute, "skill": fighterSkill, "buff": fighterBuff, "x": fighterX, "y": fighterY};
                    // add
                    fighter.push(fighterFighter);
                }
                // object
                const map = {"mapNo": mapNo, "mapId": mapId, "fighter": fighter};
                return map;
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
                    const attributeFc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const attributeHp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const attributeHealth = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const attribute = {"fc": attributeFc, "hp": attributeHp, "health": attributeHealth};
                    // 技能列表
                    const skill = [];
                    let skillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--skillLength >= 0) {
                        // 
                        // 技能ID
                        const skillSkillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const skillTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const skillNumber = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const skillBattleSkill = {"skillId": skillSkillId, "time": skillTime, "number": skillNumber};
                        // add
                        skill.push(skillBattleSkill);
                    }
                    // Buff列表
                    const buff = [];
                    let buffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--buffLength >= 0) {
                        // 
                        // BuffID
                        const buffBuffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const buffExpireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const buffOverlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const buffBattleBuff = {"buffId": buffBuffId, "expireTime": buffExpireTime, "overlap": buffOverlap};
                        // add
                        buff.push(buffBattleBuff);
                    }
                    // X坐标
                    const x = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const y = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const fighter = {"id": id, "type": type, "attribute": attribute, "skill": skill, "buff": buff, "x": x, "y": y};
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
                const fighter = {"id": id, "x": x, "y": y};
                return fighter;
            }
            case 20013: {
                // 
                // 战斗对象ID
                const id = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const fighter = {"id": id};
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
                    const fighterListId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const fighterListType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const fighterListAttributeFc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const fighterListAttributeHp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const fighterListAttributeHealth = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const fighterListAttribute = {"fc": fighterListAttributeFc, "hp": fighterListAttributeHp, "health": fighterListAttributeHealth};
                    // 技能列表
                    const fighterListSkill = [];
                    let fighterListSkillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--fighterListSkillLength >= 0) {
                        // 
                        // 技能ID
                        const fighterListSkillSkillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const fighterListSkillTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const fighterListSkillNumber = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const fighterListSkillBattleSkill = {"skillId": fighterListSkillSkillId, "time": fighterListSkillTime, "number": fighterListSkillNumber};
                        // add
                        fighterListSkill.push(fighterListSkillBattleSkill);
                    }
                    // Buff列表
                    const fighterListBuff = [];
                    let fighterListBuffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--fighterListBuffLength >= 0) {
                        // 
                        // BuffID
                        const fighterListBuffBuffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const fighterListBuffExpireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const fighterListBuffOverlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const fighterListBuffBattleBuff = {"buffId": fighterListBuffBuffId, "expireTime": fighterListBuffExpireTime, "overlap": fighterListBuffOverlap};
                        // add
                        fighterListBuff.push(fighterListBuffBattleBuff);
                    }
                    // X坐标
                    const fighterListX = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const fighterListY = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const fighterListFighter = {"id": fighterListId, "type": fighterListType, "attribute": fighterListAttribute, "skill": fighterListSkill, "buff": fighterListBuff, "x": fighterListX, "y": fighterListY};
                    // add
                    fighterList.push(fighterListFighter);
                }
                // object
                const data = {"fighterId": fighterId, "performSkillId": performSkillId, "fighterList": fighterList};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}