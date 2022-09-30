export function encodeWelfareProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 15002: {
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 兑换码
            const keyArray = textEncoder.encode(data["key"]);
            view.setUint16(offset, keyArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + keyArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(keyArray);
            offset = offset + keyArray.length;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 15003: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 红包编号
            view.setBigUint64(offset, data["luckyMoneyNo"], false);
            offset = offset + 8;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 15004: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 红包编号
            view.setBigUint64(offset, data["luckyMoneyNo"], false);
            offset = offset + 8;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeWelfareProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 15001: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 15002: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 15003: {
            // 红包编号
            const luckyMoneyNo = view.getBigUint64(offset, false);
            offset = offset + 8;
            // 总金币
            const totalGold = view.getBigUint64(offset, false);
            offset = offset + 8;
            // 总数量
            const totalNumber = view.getUint32(offset, false);
            offset = offset + 4;
            // 已经领取人数
            const receiveNumber = view.getUint16(offset, false);
            offset = offset + 2;
            // 领取列表
            const receiveList = [];
            let receiveListLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--receiveListLength >= 0) {
                // 服务器Id
                const serverId = view.getUint16(offset, false);
                offset = offset + 2;
                // 角色Id
                const roleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名
                const roleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                const roleName = textDecoder.decode(roleNameArray);
                offset = offset + roleNameLength;
                // 金币
                const gold = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 领取时间
                const receiveTime = view.getUint32(offset, false);
                offset = offset + 4;
                // add
                receiveList.push({serverId, roleId, roleName, gold, receiveTime});
            }
            // 发送时间
            const sendTime = view.getUint32(offset, false);
            offset = offset + 4;
            return {luckyMoneyNo, totalGold, totalNumber, receiveNumber, receiveList, sendTime};
        }
        case 15004: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 金币
            const gold = view.getBigUint64(offset, false);
            offset = offset + 8;
            return {result, gold};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}