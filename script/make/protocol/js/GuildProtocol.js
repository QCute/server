export class GuildQueryGuildRequest {
    /** @type {number} protocol **/
    protocol = 30101;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildQueryGuildResponse {
    /** @type {number} protocol **/
    protocol = 30101;
    /**
     * @type {Array<{
     *     guildId: BigInt;                                                                         // 公会ID
     *     guildName: string;                                                                       // 公会名字
     *     createTime: number;                                                                      // 创建时间
     *     leaderRoleId: BigInt;                                                                    // 会长角色ID
     *     leaderName: string;                                                                      // 会长名字
     * }>} data
    **/
    data;
}

export class GuildQueryRoleRequest {
    /** @type {number} protocol **/
    protocol = 30102;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildQueryRoleResponse {
    /** @type {number} protocol **/
    protocol = 30102;
    /**
     * @type {Array<{
     *     roleId: BigInt;                                                                          // 成员ID
     *     job: number;                                                                             // 职位
     *     joinTime: number;                                                                        // 加入时间
     *     roleName: string;                                                                        // 成员名字
     *     sex: number;                                                                             // 性别
     *     classes: number;                                                                         // 职业
     *     vipLevel: number;                                                                        // Vip等级
     * }>} data
    **/
    data;
}

export class GuildQueryApplyRequest {
    /** @type {number} protocol **/
    protocol = 30103;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildQueryApplyResponse {
    /** @type {number} protocol **/
    protocol = 30103;
    /**
     * @type {Array<{
     *     roleId: BigInt;                                                                          // 申请ID
     *     applyTime: number;                                                                       // 申请时间
     *     roleName: string;                                                                        // 申请名字
     *     sex: number;                                                                             // 性别
     *     classes: number;                                                                         // 职业
     *     vipLevel: number;                                                                        // Vip等级
     * }>} data
    **/
    data;
}

export class GuildQuerySelfGuildRequest {
    /** @type {number} protocol **/
    protocol = 30104;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildQuerySelfGuildResponse {
    /** @type {number} protocol **/
    protocol = 30104;
    /**
     * @type {{
     *     guildId: BigInt;                                                                         // 公会ID
     *     guildName: string;                                                                       // 公会名字
     *     exp: number;                                                                             // 经验
     *     wealth: number;                                                                          // 财富
     *     level: number;                                                                           // 等级
     *     createTime: number;                                                                      // 创建时间
     *     notice: string;                                                                          // 公告
     *     leaderRoleId: BigInt;                                                                    // 会长角色ID
     *     leaderName: string;                                                                      // 会长名字
     * }} data
    **/
    data;
}

export class GuildQuerySelfRoleRequest {
    /** @type {number} protocol **/
    protocol = 30105;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildQuerySelfRoleResponse {
    /** @type {number} protocol **/
    protocol = 30105;
    /**
     * @type {{
     *     roleId: BigInt;                                                                          // 成员ID
     *     job: number;                                                                             // 职位
     *     joinTime: number;                                                                        // 加入时间
     *     roleName: string;                                                                        // 成员名字
     *     sex: number;                                                                             // 性别
     *     classes: number;                                                                         // 职业
     *     vipLevel: number;                                                                        // Vip等级
     * }} data
    **/
    data;
}

export class GuildQuerySelfApplyRequest {
    /** @type {number} protocol **/
    protocol = 30106;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildQuerySelfApplyResponse {
    /** @type {number} protocol **/
    protocol = 30106;
    /**
     * @type {Array<{
     *     guildId: BigInt;                                                                         // 公会ID
     *     applyTime: number;                                                                       // 申请时间
     *     guildName: string;                                                                       // 公会名字
     * }>} data
    **/
    data;
}

export class GuildCreateRequest {
    /** @type {number} protocol **/
    protocol = 30107;
    /**
     * @type {{
     *     type: number;                                                                            // 类型
     *     guildName: string;                                                                       // 公会名
     * }} data
    **/
    data;
}

export class GuildCreateResponse {
    /** @type {number} protocol **/
    protocol = 30107;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildApplyRequest {
    /** @type {number} protocol **/
    protocol = 30108;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class GuildApplyResponse {
    /** @type {number} protocol **/
    protocol = 30108;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildCancelApplyRequest {
    /** @type {number} protocol **/
    protocol = 30109;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class GuildCancelApplyResponse {
    /** @type {number} protocol **/
    protocol = 30109;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildCancelAllApplyRequest {
    /** @type {number} protocol **/
    protocol = 30110;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildCancelAllApplyResponse {
    /** @type {number} protocol **/
    protocol = 30110;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildApproveApplyRequest {
    /** @type {number} protocol **/
    protocol = 30111;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class GuildApproveApplyResponse {
    /** @type {number} protocol **/
    protocol = 30111;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildApproveAllApplyRequest {
    /** @type {number} protocol **/
    protocol = 30112;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildApproveAllApplyResponse {
    /** @type {number} protocol **/
    protocol = 30112;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildRejectApplyRequest {
    /** @type {number} protocol **/
    protocol = 30113;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class GuildRejectApplyResponse {
    /** @type {number} protocol **/
    protocol = 30113;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildRejectAllApplyRequest {
    /** @type {number} protocol **/
    protocol = 30114;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildRejectAllApplyResponse {
    /** @type {number} protocol **/
    protocol = 30114;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildLeaveRequest {
    /** @type {number} protocol **/
    protocol = 30115;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildLeaveResponse {
    /** @type {number} protocol **/
    protocol = 30115;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildDismissRequest {
    /** @type {number} protocol **/
    protocol = 30116;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildDismissResponse {
    /** @type {number} protocol **/
    protocol = 30116;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildKickRequest {
    /** @type {number} protocol **/
    protocol = 30117;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class GuildKickResponse {
    /** @type {number} protocol **/
    protocol = 30117;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildUpdateJobRequest {
    /** @type {number} protocol **/
    protocol = 30118;
    /**
     * @type {{
     *     roleId: BigInt;                                                                          // 角色ID
     *     job: number;                                                                             // 位置
     * }} data
    **/
    data;
}

export class GuildUpdateJobResponse {
    /** @type {number} protocol **/
    protocol = 30118;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildUpgradeLevelRequest {
    /** @type {number} protocol **/
    protocol = 30119;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class GuildUpgradeLevelResponse {
    /** @type {number} protocol **/
    protocol = 30119;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildChangeNoticeRequest {
    /** @type {number} protocol **/
    protocol = 30120;
    /**
     * @type {string} data
    **/
    data;
}

export class GuildChangeNoticeResponse {
    /** @type {number} protocol **/
    protocol = 30120;
    /**
     * @type {string} data
    **/
    data;
}

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
                view.setUint8(offset, data.type, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 公会名
                const dataGuildNameArray = textEncoder.encode(data.guildName);
                view.setUint16(offset, dataGuildNameArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataGuildNameArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataGuildNameArray);
                offset = offset + dataGuildNameArray.length;
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
                view.setBigUint64(offset, data.roleId, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 位置
                view.setUint8(offset, data.job, false);
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
                    const dataDataGuildId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 公会名字
                    const dataDataGuildNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataGuildNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataGuildNameLength));
                    const dataDataGuildName = textDecoder.decode(dataDataGuildNameArray);
                    offset = offset + dataDataGuildNameLength;
                    // 创建时间
                    const dataDataCreateTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 会长角色ID
                    const dataDataLeaderRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 会长名字
                    const dataDataLeaderNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataLeaderNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataLeaderNameLength));
                    const dataDataLeaderName = textDecoder.decode(dataDataLeaderNameArray);
                    offset = offset + dataDataLeaderNameLength;
                    // object
                    const dataData = {"guildId": dataDataGuildId, "guildName": dataDataGuildName, "createTime": dataDataCreateTime, "leaderRoleId": dataDataLeaderRoleId, "leaderName": dataDataLeaderName};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 30101, "data": data};
            }
            case 30102: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 成员ID
                    const dataDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 职位
                    const dataDataJob = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 加入时间
                    const dataDataJoinTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 成员名字
                    const dataDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataRoleNameLength));
                    const dataDataRoleName = textDecoder.decode(dataDataRoleNameArray);
                    offset = offset + dataDataRoleNameLength;
                    // 性别
                    const dataDataSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 职业
                    const dataDataClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // Vip等级
                    const dataDataVipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataData = {"roleId": dataDataRoleId, "job": dataDataJob, "joinTime": dataDataJoinTime, "roleName": dataDataRoleName, "sex": dataDataSex, "classes": dataDataClasses, "vipLevel": dataDataVipLevel};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 30102, "data": data};
            }
            case 30103: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 申请ID
                    const dataDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 申请时间
                    const dataDataApplyTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 申请名字
                    const dataDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataRoleNameLength));
                    const dataDataRoleName = textDecoder.decode(dataDataRoleNameArray);
                    offset = offset + dataDataRoleNameLength;
                    // 性别
                    const dataDataSex = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 职业
                    const dataDataClasses = view.getUint8(offset, false);
                    offset = offset + 1;
                    // Vip等级
                    const dataDataVipLevel = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataData = {"roleId": dataDataRoleId, "applyTime": dataDataApplyTime, "roleName": dataDataRoleName, "sex": dataDataSex, "classes": dataDataClasses, "vipLevel": dataDataVipLevel};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 30103, "data": data};
            }
            case 30104: {
                // 
                // 公会ID
                const dataGuildId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 公会名字
                const dataGuildNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataGuildNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataGuildNameLength));
                const dataGuildName = textDecoder.decode(dataGuildNameArray);
                offset = offset + dataGuildNameLength;
                // 经验
                const dataExp = view.getUint32(offset, false);
                offset = offset + 4;
                // 财富
                const dataWealth = view.getUint32(offset, false);
                offset = offset + 4;
                // 等级
                const dataLevel = view.getUint8(offset, false);
                offset = offset + 1;
                // 创建时间
                const dataCreateTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 公告
                const dataNoticeLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataNoticeArray = new Uint8Array(view.buffer.slice(offset, offset + dataNoticeLength));
                const dataNotice = textDecoder.decode(dataNoticeArray);
                offset = offset + dataNoticeLength;
                // 会长角色ID
                const dataLeaderRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 会长名字
                const dataLeaderNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataLeaderNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataLeaderNameLength));
                const dataLeaderName = textDecoder.decode(dataLeaderNameArray);
                offset = offset + dataLeaderNameLength;
                // object
                const data = {"guildId": dataGuildId, "guildName": dataGuildName, "exp": dataExp, "wealth": dataWealth, "level": dataLevel, "createTime": dataCreateTime, "notice": dataNotice, "leaderRoleId": dataLeaderRoleId, "leaderName": dataLeaderName};
                return {"protocol": 30104, "data": data};
            }
            case 30105: {
                // 
                // 成员ID
                const dataRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 职位
                const dataJob = view.getUint8(offset, false);
                offset = offset + 1;
                // 加入时间
                const dataJoinTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 成员名字
                const dataRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataRoleNameLength));
                const dataRoleName = textDecoder.decode(dataRoleNameArray);
                offset = offset + dataRoleNameLength;
                // 性别
                const dataSex = view.getUint8(offset, false);
                offset = offset + 1;
                // 职业
                const dataClasses = view.getUint8(offset, false);
                offset = offset + 1;
                // Vip等级
                const dataVipLevel = view.getUint8(offset, false);
                offset = offset + 1;
                // object
                const data = {"roleId": dataRoleId, "job": dataJob, "joinTime": dataJoinTime, "roleName": dataRoleName, "sex": dataSex, "classes": dataClasses, "vipLevel": dataVipLevel};
                return {"protocol": 30105, "data": data};
            }
            case 30106: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 公会ID
                    const dataDataGuildId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 申请时间
                    const dataDataApplyTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 公会名字
                    const dataDataGuildNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataGuildNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataGuildNameLength));
                    const dataDataGuildName = textDecoder.decode(dataDataGuildNameArray);
                    offset = offset + dataDataGuildNameLength;
                    // object
                    const dataData = {"guildId": dataDataGuildId, "applyTime": dataDataApplyTime, "guildName": dataDataGuildName};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 30106, "data": data};
            }
            case 30107: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30107, "data": data};
            }
            case 30108: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30108, "data": data};
            }
            case 30109: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30109, "data": data};
            }
            case 30110: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30110, "data": data};
            }
            case 30111: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30111, "data": data};
            }
            case 30112: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30112, "data": data};
            }
            case 30113: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30113, "data": data};
            }
            case 30114: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30114, "data": data};
            }
            case 30115: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30115, "data": data};
            }
            case 30116: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30116, "data": data};
            }
            case 30117: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30117, "data": data};
            }
            case 30118: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30118, "data": data};
            }
            case 30119: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30119, "data": data};
            }
            case 30120: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 30120, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}