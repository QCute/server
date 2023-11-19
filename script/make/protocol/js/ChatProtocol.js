export class ChatGetSystemListRequest {
    /** @type {number} protocol **/
    protocol = 11602;
    /**
     * @type {number} data
    **/
    data;
}

export class ChatGetSystemListResponse {
    /** @type {number} protocol **/
    protocol = 11602;
    /**
     * @type {Array<{
     *     id: BigInt;                                                                              // ID
     *     roleId: BigInt;                                                                          // 角色ID
     *     roleName: string;                                                                        // 角色名字
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息内容
     * }>} data
    **/
    data;
}

export class ChatWorldRequest {
    /** @type {number} protocol **/
    protocol = 11603;
    /**
     * @type {{
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息
     * }} data
    **/
    data;
}

export class ChatWorldResponse {
    /** @type {number} protocol **/
    protocol = 11603;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     worldChat: {
     *         id: BigInt;                                                                          // ID
     *         roleId: BigInt;                                                                      // 角色ID
     *         roleName: string;                                                                    // 角色名字
     *         type: number;                                                                        // 类型
     *         message: string;                                                                     // 消息内容
     *     };                                                                                       // 
     * }} data
    **/
    data;
}

export class ChatGetWorldListRequest {
    /** @type {number} protocol **/
    protocol = 11604;
    /**
     * @type {number} data
    **/
    data;
}

export class ChatGetWorldListResponse {
    /** @type {number} protocol **/
    protocol = 11604;
    /**
     * @type {Array<{
     *     id: BigInt;                                                                              // ID
     *     roleId: BigInt;                                                                          // 角色ID
     *     roleName: string;                                                                        // 角色名字
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息内容
     * }>} data
    **/
    data;
}

export class ChatGuildRequest {
    /** @type {number} protocol **/
    protocol = 11605;
    /**
     * @type {{
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息
     * }} data
    **/
    data;
}

export class ChatGuildResponse {
    /** @type {number} protocol **/
    protocol = 11605;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     guildChat: {
     *         id: BigInt;                                                                          // ID
     *         roleId: BigInt;                                                                      // 角色ID
     *         roleName: string;                                                                    // 角色名字
     *         type: number;                                                                        // 类型
     *         message: string;                                                                     // 消息内容
     *     };                                                                                       // 
     * }} data
    **/
    data;
}

export class ChatGetGuildListRequest {
    /** @type {number} protocol **/
    protocol = 11606;
    /**
     * @type {number} data
    **/
    data;
}

export class ChatGetGuildListResponse {
    /** @type {number} protocol **/
    protocol = 11606;
    /**
     * @type {Array<{
     *     id: BigInt;                                                                              // ID
     *     roleId: BigInt;                                                                          // 角色ID
     *     roleName: string;                                                                        // 角色名字
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息内容
     * }>} data
    **/
    data;
}

export class ChatPrivateRequest {
    /** @type {number} protocol **/
    protocol = 11607;
    /**
     * @type {{
     *     roleId: BigInt;                                                                          // 角色ID
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息
     * }} data
    **/
    data;
}

export class ChatPrivateResponse {
    /** @type {number} protocol **/
    protocol = 11607;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     privateChat: {
     *         senderId: BigInt;                                                                    // 发送者角色ID
     *         receiverId: BigInt;                                                                  // 接收者角色ID
     *         type: number;                                                                        // 类型
     *         message: string;                                                                     // 消息内容
     *     };                                                                                       // 
     * }} data
    **/
    data;
}

export class ChatGetPrivateListRequest {
    /** @type {number} protocol **/
    protocol = 11608;
    /**
     * @type {{
     *     roleId: BigInt;                                                                          // 角色ID
     *     page: number;                                                                            // 页
     * }} data
    **/
    data;
}

export class ChatGetPrivateListResponse {
    /** @type {number} protocol **/
    protocol = 11608;
    /**
     * @type {Array<{
     *     senderId: BigInt;                                                                        // 发送者角色ID
     *     receiverId: BigInt;                                                                      // 接收者角色ID
     *     type: number;                                                                            // 类型
     *     message: string;                                                                         // 消息内容
     * }>} data
    **/
    data;
}

export default class ChatProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11602: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 页
                view.setUint16(offset, data, false);
                offset = offset + 2;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11603: {

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
                // 消息
                const dataMessageArray = textEncoder.encode(data.message);
                view.setUint16(offset, dataMessageArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataMessageArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataMessageArray);
                offset = offset + dataMessageArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11604: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 页
                view.setUint16(offset, data, false);
                offset = offset + 2;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11605: {

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
                // 消息
                const dataMessageArray = textEncoder.encode(data.message);
                view.setUint16(offset, dataMessageArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataMessageArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataMessageArray);
                offset = offset + dataMessageArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11606: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 页
                view.setUint16(offset, data, false);
                offset = offset + 2;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11607: {

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
                // 类型
                view.setUint8(offset, data.type, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 消息
                const dataMessageArray = textEncoder.encode(data.message);
                view.setUint16(offset, dataMessageArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataMessageArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataMessageArray);
                offset = offset + dataMessageArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11608: {

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
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 页
                view.setUint16(offset, data.page, false);
                offset = offset + 2;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11602: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const dataDataId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色ID
                    const dataDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名字
                    const dataDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataRoleNameLength));
                    const dataDataRoleName = textDecoder.decode(dataDataRoleNameArray);
                    offset = offset + dataDataRoleNameLength;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const dataDataMessageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataMessageLength));
                    const dataDataMessage = textDecoder.decode(dataDataMessageArray);
                    offset = offset + dataDataMessageLength;
                    // object
                    const dataData = {"id": dataDataId, "roleId": dataDataRoleId, "roleName": dataDataRoleName, "type": dataDataType, "message": dataDataMessage};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11602, "data": data};
            }
            case 11603: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 
                // ID
                const dataWorldChatId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色ID
                const dataWorldChatRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名字
                const dataWorldChatRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataWorldChatRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataWorldChatRoleNameLength));
                const dataWorldChatRoleName = textDecoder.decode(dataWorldChatRoleNameArray);
                offset = offset + dataWorldChatRoleNameLength;
                // 类型
                const dataWorldChatType = view.getUint8(offset, false);
                offset = offset + 1;
                // 消息内容
                const dataWorldChatMessageLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataWorldChatMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataWorldChatMessageLength));
                const dataWorldChatMessage = textDecoder.decode(dataWorldChatMessageArray);
                offset = offset + dataWorldChatMessageLength;
                // object
                const dataWorldChat = {"id": dataWorldChatId, "roleId": dataWorldChatRoleId, "roleName": dataWorldChatRoleName, "type": dataWorldChatType, "message": dataWorldChatMessage};
                // object
                const data = {"result": dataResult, "worldChat": dataWorldChat};
                return {"protocol": 11603, "data": data};
            }
            case 11604: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const dataDataId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色ID
                    const dataDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名字
                    const dataDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataRoleNameLength));
                    const dataDataRoleName = textDecoder.decode(dataDataRoleNameArray);
                    offset = offset + dataDataRoleNameLength;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const dataDataMessageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataMessageLength));
                    const dataDataMessage = textDecoder.decode(dataDataMessageArray);
                    offset = offset + dataDataMessageLength;
                    // object
                    const dataData = {"id": dataDataId, "roleId": dataDataRoleId, "roleName": dataDataRoleName, "type": dataDataType, "message": dataDataMessage};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11604, "data": data};
            }
            case 11605: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 
                // ID
                const dataGuildChatId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色ID
                const dataGuildChatRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名字
                const dataGuildChatRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataGuildChatRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataGuildChatRoleNameLength));
                const dataGuildChatRoleName = textDecoder.decode(dataGuildChatRoleNameArray);
                offset = offset + dataGuildChatRoleNameLength;
                // 类型
                const dataGuildChatType = view.getUint8(offset, false);
                offset = offset + 1;
                // 消息内容
                const dataGuildChatMessageLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataGuildChatMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataGuildChatMessageLength));
                const dataGuildChatMessage = textDecoder.decode(dataGuildChatMessageArray);
                offset = offset + dataGuildChatMessageLength;
                // object
                const dataGuildChat = {"id": dataGuildChatId, "roleId": dataGuildChatRoleId, "roleName": dataGuildChatRoleName, "type": dataGuildChatType, "message": dataGuildChatMessage};
                // object
                const data = {"result": dataResult, "guildChat": dataGuildChat};
                return {"protocol": 11605, "data": data};
            }
            case 11606: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const dataDataId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色ID
                    const dataDataRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名字
                    const dataDataRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataRoleNameLength));
                    const dataDataRoleName = textDecoder.decode(dataDataRoleNameArray);
                    offset = offset + dataDataRoleNameLength;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const dataDataMessageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataMessageLength));
                    const dataDataMessage = textDecoder.decode(dataDataMessageArray);
                    offset = offset + dataDataMessageLength;
                    // object
                    const dataData = {"id": dataDataId, "roleId": dataDataRoleId, "roleName": dataDataRoleName, "type": dataDataType, "message": dataDataMessage};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11606, "data": data};
            }
            case 11607: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 
                // 发送者角色ID
                const dataPrivateChatSenderId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 接收者角色ID
                const dataPrivateChatReceiverId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 类型
                const dataPrivateChatType = view.getUint8(offset, false);
                offset = offset + 1;
                // 消息内容
                const dataPrivateChatMessageLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataPrivateChatMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataPrivateChatMessageLength));
                const dataPrivateChatMessage = textDecoder.decode(dataPrivateChatMessageArray);
                offset = offset + dataPrivateChatMessageLength;
                // object
                const dataPrivateChat = {"senderId": dataPrivateChatSenderId, "receiverId": dataPrivateChatReceiverId, "type": dataPrivateChatType, "message": dataPrivateChatMessage};
                // object
                const data = {"result": dataResult, "privateChat": dataPrivateChat};
                return {"protocol": 11607, "data": data};
            }
            case 11608: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 发送者角色ID
                    const dataDataSenderId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 接收者角色ID
                    const dataDataReceiverId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const dataDataMessageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataMessageArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataMessageLength));
                    const dataDataMessage = textDecoder.decode(dataDataMessageArray);
                    offset = offset + dataDataMessageLength;
                    // object
                    const dataData = {"senderId": dataDataSenderId, "receiverId": dataDataReceiverId, "type": dataDataType, "message": dataDataMessage};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11608, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}