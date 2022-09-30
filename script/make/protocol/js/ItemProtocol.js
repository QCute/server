export function encodeItemProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 11106: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 物品编号
            view.setBigUint64(offset, data["itemNo"], false);
            offset = offset + 8;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 数量
            view.setUint16(offset, data["number"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 1) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 类型
            view.setUint8(offset, data["type"], false);
            offset = offset + 1;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeItemProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 11101: {
            // 道具列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 物品编号
                const itemNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 物品ID
                const itemId = view.getUint32(offset, false);
                offset = offset + 4;
                // 类型
                const type = view.getUint8(offset, false);
                offset = offset + 1;
                // 数量
                const number = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({itemNo, itemId, type, number});
            }
            return {list};
        }
        case 11102: {
            // 背包列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 物品编号
                const itemNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 物品ID
                const itemId = view.getUint32(offset, false);
                offset = offset + 4;
                // 类型
                const type = view.getUint8(offset, false);
                offset = offset + 1;
                // 数量
                const number = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({itemNo, itemId, type, number});
            }
            return {list};
        }
        case 11103: {
            // 仓库列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 物品编号
                const itemNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 物品ID
                const itemId = view.getUint32(offset, false);
                offset = offset + 4;
                // 类型
                const type = view.getUint8(offset, false);
                offset = offset + 1;
                // 数量
                const number = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({itemNo, itemId, type, number});
            }
            return {list};
        }
        case 11104: {
            // 删除列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 物品编号
                const itemNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 类型
                const type = view.getUint8(offset, false);
                offset = offset + 1;
                // add
                list.push({itemNo, type});
            }
            return {list};
        }
        case 11106: {
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