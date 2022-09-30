export function encodeAccountProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 10001: {
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 服务器ID
            view.setUint16(offset, data["serverId"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 账户名
            const accountNameArray = textEncoder.encode(data["accountName"]);
            view.setUint16(offset, accountNameArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + accountNameArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(accountNameArray);
            offset = offset + accountNameArray.length;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 10002: {
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 角色名
            const roleNameArray = textEncoder.encode(data["roleName"]);
            view.setUint16(offset, roleNameArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + roleNameArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(roleNameArray);
            offset = offset + roleNameArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 服务器ID
            view.setUint16(offset, data["serverId"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 账户名
            const accountNameArray = textEncoder.encode(data["accountName"]);
            view.setUint16(offset, accountNameArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + accountNameArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(accountNameArray);
            offset = offset + accountNameArray.length;
            // extend
            while (view.byteLength < offset + 1) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 性别
            view.setUint8(offset, data["sex"], false);
            offset = offset + 1;
            // extend
            while (view.byteLength < offset + 1) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 职业
            view.setUint8(offset, data["classes"], false);
            offset = offset + 1;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 渠道
            const channelArray = textEncoder.encode(data["channel"]);
            view.setUint16(offset, channelArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + channelArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(channelArray);
            offset = offset + channelArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 设备
            const deviceIdArray = textEncoder.encode(data["deviceId"]);
            view.setUint16(offset, deviceIdArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + deviceIdArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(deviceIdArray);
            offset = offset + deviceIdArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // mac地址
            const macArray = textEncoder.encode(data["mac"]);
            view.setUint16(offset, macArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + macArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(macArray);
            offset = offset + macArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 设备类型
            const deviceTypeArray = textEncoder.encode(data["deviceType"]);
            view.setUint16(offset, deviceTypeArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + deviceTypeArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(deviceTypeArray);
            offset = offset + deviceTypeArray.length;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 10003: {
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
            // 角色名
            const roleNameArray = textEncoder.encode(data["roleName"]);
            view.setUint16(offset, roleNameArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + roleNameArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(roleNameArray);
            offset = offset + roleNameArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 服务器ID
            view.setUint16(offset, data["serverId"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 账户名
            const accountNameArray = textEncoder.encode(data["accountName"]);
            view.setUint16(offset, accountNameArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + accountNameArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(accountNameArray);
            offset = offset + accountNameArray.length;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeAccountProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 10000: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 10001: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 角色名列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 角色ID
                const roleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名
                const roleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                const roleName = textDecoder.decode(roleNameArray);
                offset = offset + roleNameLength;
                // add
                list.push({roleId, roleName});
            }
            return {result, list};
        }
        case 10002: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 角色ID
            const roleId = view.getBigUint64(offset, false);
            offset = offset + 8;
            // 角色名
            const roleNameLength = view.getUint16(offset, false);
            offset = offset + 2;
            const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
            const roleName = textDecoder.decode(roleNameArray);
            offset = offset + roleNameLength;
            return {result, roleId, roleName};
        }
        case 10003: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 10004: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}