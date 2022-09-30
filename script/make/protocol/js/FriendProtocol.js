export function encodeFriendProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 11502: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 好友角色ID
            view.setBigUint64(offset, data["friendRoleId"], false);
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
            view.setBigUint64(offset, data["friendRoleId"], false);
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
            view.setBigUint64(offset, data["friendRoleId"], false);
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
            view.setBigUint64(offset, data["friendRoleId"], false);
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
            view.setBigUint64(offset, data["friendRoleId"], false);
            offset = offset + 8;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeFriendProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 11501: {
            // 好友列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
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
                // add
                list.push({friendRoleId, friendName, relation, time});
            }
            return {list};
        }
        case 11502: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 11503: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 11504: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 好友角色ID
            const friendRoleId = view.getBigUint64(offset, false);
            offset = offset + 8;
            return {result, friendRoleId};
        }
        case 11505: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 好友角色ID
            const friendRoleId = view.getBigUint64(offset, false);
            offset = offset + 8;
            return {result, friendRoleId};
        }
        case 11506: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 好友角色ID
            const friendRoleId = view.getBigUint64(offset, false);
            offset = offset + 8;
            return {result, friendRoleId};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}