export default class FriendProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11501: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11502: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11503: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11504: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11505: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11506: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 好友角色ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11501: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 好友角色ID
                    const friendRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 好友名字
                    const friendNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const friendNameArray = new Uint8Array(view.buffer.slice(offset, offset + friendNameLength));
                    const friendName = textDecoder.decode(friendNameArray);
                    offset = offset + friendNameLength;
                    // 关系状态(申请:1/好友:2/黑名单:3)
                    const relation = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 添加/修改状态时间
                    const time = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const friend = {"friendRoleId": friendRoleId, "friendName": friendName, "relation": relation, "time": time};
                    // add
                    data.push(friend);
                }
                return data;
            }
            case 11502: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 11503: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 11504: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 好友角色ID
                const friendRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": result, "friendRoleId": friendRoleId};
                return data;
            }
            case 11505: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 好友角色ID
                const friendRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": result, "friendRoleId": friendRoleId};
                return data;
            }
            case 11506: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 好友角色ID
                const friendRoleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": result, "friendRoleId": friendRoleId};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}