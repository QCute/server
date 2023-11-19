export default class WelfareProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 15001: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 15002: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 兑换码
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
            case 15003: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 红包编号
                view.setBigUint64(offset, data, false);
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
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 15001: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 15002: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 15003: {
                // 
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
                    // 
                    // 服务器Id
                    const receiveListServerId = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 角色Id
                    const receiveListRoleId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 角色名
                    const receiveListRoleNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const receiveListRoleNameArray = new Uint8Array(view.buffer.slice(offset, offset + receiveListRoleNameLength));
                    const receiveListRoleName = textDecoder.decode(receiveListRoleNameArray);
                    offset = offset + receiveListRoleNameLength;
                    // 金币
                    const receiveListGold = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 领取时间
                    const receiveListTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const receiveListLuckyMoneyRole = {"serverId": receiveListServerId, "roleId": receiveListRoleId, "roleName": receiveListRoleName, "gold": receiveListGold, "time": receiveListTime};
                    // add
                    receiveList.push(receiveListLuckyMoneyRole);
                }
                // 发送时间
                const time = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const luckyMoney = {"luckyMoneyNo": luckyMoneyNo, "totalGold": totalGold, "totalNumber": totalNumber, "receiveNumber": receiveNumber, "receiveList": receiveList, "time": time};
                return luckyMoney;
            }
            case 15004: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 金币
                const gold = view.getBigUint64(offset, false);
                offset = offset + 8;
                // object
                const data = {"result": result, "gold": gold};
                return data;
            }
            case 15005: {
                // 

                // object
                const data = {};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}