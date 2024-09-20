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
                view.setUint16(offset, data["page"], false);
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
                view.setUint16(offset, data["page"], false);
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
                view.setUint16(offset, data["page"], false);
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
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // SystemChat
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
                    const systemChat = {id, roleId, roleName, type, message};
                    // add
                    list.push(systemChat);
                }
                return {list};
            }
            case 11603: {
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // WorldChat
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
                const worldChat = {id, roleId, roleName, type, message};
                return {result, worldChat};
            }
            case 11604: {
                // 
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // WorldChat
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
                    const worldChat = {id, roleId, roleName, type, message};
                    // add
                    list.push(worldChat);
                }
                return {list};
            }
            case 11605: {
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // GuildChat
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
                const guildChat = {id, roleId, roleName, type, message};
                return {result, guildChat};
            }
            case 11606: {
                // 
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // GuildChat
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
                    const guildChat = {id, roleId, roleName, type, message};
                    // add
                    list.push(guildChat);
                }
                return {list};
            }
            case 11607: {
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // PrivateChat
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
                const privateChat = {senderId, receiverId, type, message};
                return {result, privateChat};
            }
            case 11608: {
                // 
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // PrivateChat
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
                    const privateChat = {senderId, receiverId, type, message};
                    // add
                    list.push(privateChat);
                }
                return {list};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}