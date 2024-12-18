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
                view.setUint8(offset, data["type"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 消息
                const messageArray = textEncoder.encode(data["message"]);
                view.setUint16(offset, messageArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + messageArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(messageArray);
                offset = offset + messageArray.length;
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
                view.setUint8(offset, data["type"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 消息
                const messageArray = textEncoder.encode(data["message"]);
                view.setUint16(offset, messageArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + messageArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(messageArray);
                offset = offset + messageArray.length;
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
                view.setBigUint64(offset, data["roleId"], false);
                offset = offset + 8;
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
                // 消息
                const messageArray = textEncoder.encode(data["message"]);
                view.setUint16(offset, messageArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + messageArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(messageArray);
                offset = offset + messageArray.length;
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
                view.setBigUint64(offset, data["roleId"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 页
                view.setUint16(offset, data["page"], false);
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
                    const id = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色ID
                    const roleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名字
                    const roleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                    const roleName = textDecoder.decode(roleNameArray);
                    offset = offset + roleNameLength;
                    // 类型
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const messageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const messageArray = new Uint8Array(view.buffer.slice(offset, offset + messageLength));
                    const message = textDecoder.decode(messageArray);
                    offset = offset + messageLength;
                    // object
                    const systemChat = {"id": id, "roleId": roleId, "roleName": roleName, "type": type, "message": message};
                    // add
                    data.push(systemChat);
                }
                return data;
            }
            case 11603: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 
                // ID
                const worldChatId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色ID
                const worldChatRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名字
                const worldChatRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const worldChatRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + worldChatRoleNameLength));
                const worldChatRoleName = textDecoder.decode(worldChatRoleNameArray);
                offset = offset + worldChatRoleNameLength;
                // 类型
                const worldChatType = view.getUint8(offset, false);
                offset = offset + 1;
                // 消息内容
                const worldChatMessageLength = view.getUint16(offset, false);
                offset = offset + 2;
                const worldChatMessageArray = new Uint8Array(view.buffer.slice(offset, offset + worldChatMessageLength));
                const worldChatMessage = textDecoder.decode(worldChatMessageArray);
                offset = offset + worldChatMessageLength;
                // object
                const worldChat = {"id": worldChatId, "roleId": worldChatRoleId, "roleName": worldChatRoleName, "type": worldChatType, "message": worldChatMessage};
                // object
                const data = {"result": result, "worldChat": worldChat};
                return data;
            }
            case 11604: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const id = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色ID
                    const roleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名字
                    const roleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                    const roleName = textDecoder.decode(roleNameArray);
                    offset = offset + roleNameLength;
                    // 类型
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const messageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const messageArray = new Uint8Array(view.buffer.slice(offset, offset + messageLength));
                    const message = textDecoder.decode(messageArray);
                    offset = offset + messageLength;
                    // object
                    const worldChat = {"id": id, "roleId": roleId, "roleName": roleName, "type": type, "message": message};
                    // add
                    data.push(worldChat);
                }
                return data;
            }
            case 11605: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 
                // ID
                const guildChatId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色ID
                const guildChatRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名字
                const guildChatRoleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const guildChatRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + guildChatRoleNameLength));
                const guildChatRoleName = textDecoder.decode(guildChatRoleNameArray);
                offset = offset + guildChatRoleNameLength;
                // 类型
                const guildChatType = view.getUint8(offset, false);
                offset = offset + 1;
                // 消息内容
                const guildChatMessageLength = view.getUint16(offset, false);
                offset = offset + 2;
                const guildChatMessageArray = new Uint8Array(view.buffer.slice(offset, offset + guildChatMessageLength));
                const guildChatMessage = textDecoder.decode(guildChatMessageArray);
                offset = offset + guildChatMessageLength;
                // object
                const guildChat = {"id": guildChatId, "roleId": guildChatRoleId, "roleName": guildChatRoleName, "type": guildChatType, "message": guildChatMessage};
                // object
                const data = {"result": result, "guildChat": guildChat};
                return data;
            }
            case 11606: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // ID
                    const id = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色ID
                    const roleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名字
                    const roleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                    const roleName = textDecoder.decode(roleNameArray);
                    offset = offset + roleNameLength;
                    // 类型
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const messageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const messageArray = new Uint8Array(view.buffer.slice(offset, offset + messageLength));
                    const message = textDecoder.decode(messageArray);
                    offset = offset + messageLength;
                    // object
                    const guildChat = {"id": id, "roleId": roleId, "roleName": roleName, "type": type, "message": message};
                    // add
                    data.push(guildChat);
                }
                return data;
            }
            case 11607: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 
                // 发送者角色ID
                const privateChatSenderId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 接收者角色ID
                const privateChatReceiverId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 类型
                const privateChatType = view.getUint8(offset, false);
                offset = offset + 1;
                // 消息内容
                const privateChatMessageLength = view.getUint16(offset, false);
                offset = offset + 2;
                const privateChatMessageArray = new Uint8Array(view.buffer.slice(offset, offset + privateChatMessageLength));
                const privateChatMessage = textDecoder.decode(privateChatMessageArray);
                offset = offset + privateChatMessageLength;
                // object
                const privateChat = {"senderId": privateChatSenderId, "receiverId": privateChatReceiverId, "type": privateChatType, "message": privateChatMessage};
                // object
                const data = {"result": result, "privateChat": privateChat};
                return data;
            }
            case 11608: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 发送者角色ID
                    const senderId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 接收者角色ID
                    const receiverId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 类型
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 消息内容
                    const messageLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const messageArray = new Uint8Array(view.buffer.slice(offset, offset + messageLength));
                    const message = textDecoder.decode(messageArray);
                    offset = offset + messageLength;
                    // object
                    const privateChat = {"senderId": senderId, "receiverId": receiverId, "type": type, "message": message};
                    // add
                    data.push(privateChat);
                }
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}