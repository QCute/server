export class MapQueryRequest {
    /** @type {number} protocol **/
    protocol = 20001;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class MapQueryResponse {
    /** @type {number} protocol **/
    protocol = 20001;
    /**
     * @type {{
     *     mapNo: BigInt;                                                                           // 地图编号
     *     mapId: number;                                                                           // 地图ID
     *     fighter: Array<{
     *         id: BigInt;                                                                          // ID
     *         type: number;                                                                        // 类型
     *         attribute: {
     *             fc: BigInt;                                                                      // 战力
     *             hp: BigInt;                                                                      // 血量
     *             health: BigInt;                                                                  // 健康
     *         };                                                                                   // 属性
     *         skill: Array<{
     *             skillId: number;                                                                 // 技能ID
     *             time: number;                                                                    // 时间
     *             number: number;                                                                  // 数量
     *         }>;                                                                                  // 技能列表
     *         buff: Array<{
     *             buffId: number;                                                                  // BuffID
     *             expireTime: number;                                                              // 过期时间
     *             overlap: number;                                                                 // 数量
     *         }>;                                                                                  // Buff列表
     *         x: number;                                                                           // X坐标
     *         y: number;                                                                           // Y坐标
     *     }>;                                                                                      // 
     * }} data
    **/
    data;
}



export class MapFighterResponse {
    /** @type {number} protocol **/
    protocol = 20011;
    /**
     * @type {Array<{
     *     id: BigInt;                                                                              // ID
     *     type: number;                                                                            // 类型
     *     attribute: {
     *         fc: BigInt;                                                                          // 战力
     *         hp: BigInt;                                                                          // 血量
     *         health: BigInt;                                                                      // 健康
     *     };                                                                                       // 属性
     *     skill: Array<{
     *         skillId: number;                                                                     // 技能ID
     *         time: number;                                                                        // 时间
     *         number: number;                                                                      // 数量
     *     }>;                                                                                      // 技能列表
     *     buff: Array<{
     *         buffId: number;                                                                      // BuffID
     *         expireTime: number;                                                                  // 过期时间
     *         overlap: number;                                                                     // 数量
     *     }>;                                                                                      // Buff列表
     *     x: number;                                                                               // X坐标
     *     y: number;                                                                               // Y坐标
     * }>} data
    **/
    data;
}

export class MapFighterMoveRequest {
    /** @type {number} protocol **/
    protocol = 20012;
    /**
     * @type {{
     *     x: number;                                                                               // X坐标
     *     y: number;                                                                               // Y坐标
     * }} data
    **/
    data;
}

export class MapFighterMoveResponse {
    /** @type {number} protocol **/
    protocol = 20012;
    /**
     * @type {{
     *     id: BigInt;                                                                              // ID
     *     x: number;                                                                               // X坐标
     *     y: number;                                                                               // Y坐标
     * }} data
    **/
    data;
}



export class MapFighterLeaveResponse {
    /** @type {number} protocol **/
    protocol = 20013;
    /**
     * @type {{
     *     id: BigInt;                                                                              // 战斗对象ID
     * }} data
    **/
    data;
}

export class MapAttackRequest {
    /** @type {number} protocol **/
    protocol = 20014;
    /**
     * @type {{
     *     skillId: number;                                                                         // 技能Id
     *     targetList: Array<BigInt>;                                                               // 战斗对象ID列表
     * }} data
    **/
    data;
}

export class MapAttackResponse {
    /** @type {number} protocol **/
    protocol = 20014;
    /**
     * @type {{
     *     fighterId: BigInt;                                                                       // 战斗对象Id
     *     performSkillId: number;                                                                  // 技能Id
     *     fighterList: Array<{
     *         id: BigInt;                                                                          // ID
     *         type: number;                                                                        // 类型
     *         attribute: {
     *             fc: BigInt;                                                                      // 战力
     *             hp: BigInt;                                                                      // 血量
     *             health: BigInt;                                                                  // 健康
     *         };                                                                                   // 属性
     *         skill: Array<{
     *             skillId: number;                                                                 // 技能ID
     *             time: number;                                                                    // 时间
     *             number: number;                                                                  // 数量
     *         }>;                                                                                  // 技能列表
     *         buff: Array<{
     *             buffId: number;                                                                  // BuffID
     *             expireTime: number;                                                              // 过期时间
     *             overlap: number;                                                                 // 数量
     *         }>;                                                                                  // Buff列表
     *         x: number;                                                                           // X坐标
     *         y: number;                                                                           // Y坐标
     *     }>;                                                                                      // 
     * }} data
    **/
    data;
}

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
                view.setUint16(offset, data.x, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // Y坐标
                view.setUint16(offset, data.y, false);
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
                view.setUint32(offset, data.skillId, false);
                offset = offset + 4;
                const dataTargetList = data.targetList;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 战斗对象ID列表
                view.setUint16(offset, dataTargetList.length, false);
                offset = offset + 2;
                for (const dataTargetListData of dataTargetList) {
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 战斗对象ID
                    view.setBigUint64(offset, dataTargetListData, false);
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
                const dataMapNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 地图ID
                const dataMapId = view.getUint32(offset, false);
                offset = offset + 4;
                // 
                const dataFighter = [];
                let dataFighterLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataFighterLength >= 0) {
                    // 
                    // ID
                    const dataFighterDataId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const dataFighterDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const dataFighterDataAttributeFc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const dataFighterDataAttributeHp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const dataFighterDataAttributeHealth = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const dataFighterDataAttribute = {"fc": dataFighterDataAttributeFc, "hp": dataFighterDataAttributeHp, "health": dataFighterDataAttributeHealth};
                    // 技能列表
                    const dataFighterDataSkill = [];
                    let dataFighterDataSkillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataFighterDataSkillLength >= 0) {
                        // 
                        // 技能ID
                        const dataFighterDataSkillDataSkillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const dataFighterDataSkillDataTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataFighterDataSkillDataNumber = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const dataFighterDataSkillData = {"skillId": dataFighterDataSkillDataSkillId, "time": dataFighterDataSkillDataTime, "number": dataFighterDataSkillDataNumber};
                        // add
                        dataFighterDataSkill.push(dataFighterDataSkillData);
                    }
                    // Buff列表
                    const dataFighterDataBuff = [];
                    let dataFighterDataBuffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataFighterDataBuffLength >= 0) {
                        // 
                        // BuffID
                        const dataFighterDataBuffDataBuffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const dataFighterDataBuffDataExpireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataFighterDataBuffDataOverlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const dataFighterDataBuffData = {"buffId": dataFighterDataBuffDataBuffId, "expireTime": dataFighterDataBuffDataExpireTime, "overlap": dataFighterDataBuffDataOverlap};
                        // add
                        dataFighterDataBuff.push(dataFighterDataBuffData);
                    }
                    // X坐标
                    const dataFighterDataX = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const dataFighterDataY = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataFighterData = {"id": dataFighterDataId, "type": dataFighterDataType, "attribute": dataFighterDataAttribute, "skill": dataFighterDataSkill, "buff": dataFighterDataBuff, "x": dataFighterDataX, "y": dataFighterDataY};
                    // add
                    dataFighter.push(dataFighterData);
                }
                // object
                const data = {"mapNo": dataMapNo, "mapId": dataMapId, "fighter": dataFighter};
                return {"protocol": 20001, "data": data};
            }
            case 20011: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const dataDataId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const dataDataAttributeFc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const dataDataAttributeHp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const dataDataAttributeHealth = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const dataDataAttribute = {"fc": dataDataAttributeFc, "hp": dataDataAttributeHp, "health": dataDataAttributeHealth};
                    // 技能列表
                    const dataDataSkill = [];
                    let dataDataSkillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataDataSkillLength >= 0) {
                        // 
                        // 技能ID
                        const dataDataSkillDataSkillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const dataDataSkillDataTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataDataSkillDataNumber = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const dataDataSkillData = {"skillId": dataDataSkillDataSkillId, "time": dataDataSkillDataTime, "number": dataDataSkillDataNumber};
                        // add
                        dataDataSkill.push(dataDataSkillData);
                    }
                    // Buff列表
                    const dataDataBuff = [];
                    let dataDataBuffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataDataBuffLength >= 0) {
                        // 
                        // BuffID
                        const dataDataBuffDataBuffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const dataDataBuffDataExpireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataDataBuffDataOverlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const dataDataBuffData = {"buffId": dataDataBuffDataBuffId, "expireTime": dataDataBuffDataExpireTime, "overlap": dataDataBuffDataOverlap};
                        // add
                        dataDataBuff.push(dataDataBuffData);
                    }
                    // X坐标
                    const dataDataX = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const dataDataY = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"id": dataDataId, "type": dataDataType, "attribute": dataDataAttribute, "skill": dataDataSkill, "buff": dataDataBuff, "x": dataDataX, "y": dataDataY};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 20011, "data": data};
            }
            case 20012: {
                // 
                // ID
                const dataId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // X坐标
                const dataX = view.getUint16(offset, false);
                offset = offset + 2;
                // Y坐标
                const dataY = view.getUint16(offset, false);
                offset = offset + 2;
                // object
                const data = {"id": dataId, "x": dataX, "y": dataY};
                return {"protocol": 20012, "data": data};
            }
            case 20013: {
                // 
                // 战斗对象ID
                const dataId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"id": dataId};
                return {"protocol": 20013, "data": data};
            }
            case 20014: {
                // 
                // 战斗对象Id
                const dataFighterId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 技能Id
                const dataPerformSkillId = view.getUint32(offset, false);
                offset = offset + 4;
                // 
                const dataFighterList = [];
                let dataFighterListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataFighterListLength >= 0) {
                    // 
                    // ID
                    const dataFighterListDataId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const dataFighterListDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 属性
                    // 战力
                    const dataFighterListDataAttributeFc = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 血量
                    const dataFighterListDataAttributeHp = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 健康
                    const dataFighterListDataAttributeHealth = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // object
                    const dataFighterListDataAttribute = {"fc": dataFighterListDataAttributeFc, "hp": dataFighterListDataAttributeHp, "health": dataFighterListDataAttributeHealth};
                    // 技能列表
                    const dataFighterListDataSkill = [];
                    let dataFighterListDataSkillLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataFighterListDataSkillLength >= 0) {
                        // 
                        // 技能ID
                        const dataFighterListDataSkillDataSkillId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 时间
                        const dataFighterListDataSkillDataTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataFighterListDataSkillDataNumber = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const dataFighterListDataSkillData = {"skillId": dataFighterListDataSkillDataSkillId, "time": dataFighterListDataSkillDataTime, "number": dataFighterListDataSkillDataNumber};
                        // add
                        dataFighterListDataSkill.push(dataFighterListDataSkillData);
                    }
                    // Buff列表
                    const dataFighterListDataBuff = [];
                    let dataFighterListDataBuffLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataFighterListDataBuffLength >= 0) {
                        // 
                        // BuffID
                        const dataFighterListDataBuffDataBuffId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 过期时间
                        const dataFighterListDataBuffDataExpireTime = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataFighterListDataBuffDataOverlap = view.getUint32(offset, false);
                        offset = offset + 4;
                        // object
                        const dataFighterListDataBuffData = {"buffId": dataFighterListDataBuffDataBuffId, "expireTime": dataFighterListDataBuffDataExpireTime, "overlap": dataFighterListDataBuffDataOverlap};
                        // add
                        dataFighterListDataBuff.push(dataFighterListDataBuffData);
                    }
                    // X坐标
                    const dataFighterListDataX = view.getUint16(offset, false);
                    offset = offset + 2;
                    // Y坐标
                    const dataFighterListDataY = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataFighterListData = {"id": dataFighterListDataId, "type": dataFighterListDataType, "attribute": dataFighterListDataAttribute, "skill": dataFighterListDataSkill, "buff": dataFighterListDataBuff, "x": dataFighterListDataX, "y": dataFighterListDataY};
                    // add
                    dataFighterList.push(dataFighterListData);
                }
                // object
                const data = {"fighterId": dataFighterId, "performSkillId": dataPerformSkillId, "fighterList": dataFighterList};
                return {"protocol": 20014, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}