export function encodeMailProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 11402: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 邮件ID
            view.setBigUint64(offset, data["mailId"], false);
            offset = offset + 8;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 11403: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 邮件ID
            view.setBigUint64(offset, data["mailId"], false);
            offset = offset + 8;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 11404: {
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 邮件ID
            view.setBigUint64(offset, data["mailId"], false);
            offset = offset + 8;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeMailProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 11401: {
            // 邮件列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 邮件ID
                const mailId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 接收时间
                const receiveTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 有效时间
                const expireTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 读取时间
                const readTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 领取附件时间
                const receiveAttachmentTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 标题
                const titleLength = view.getUint16(offset, false);
                offset = offset + 2;
                const titleArray = new Uint8Array(view.buffer.slice(offset, offset + titleLength));
                const title = textDecoder.decode(titleArray);
                offset = offset + titleLength;
                // 内容
                const contentLength = view.getUint16(offset, false);
                offset = offset + 2;
                const contentArray = new Uint8Array(view.buffer.slice(offset, offset + contentLength));
                const content = textDecoder.decode(contentArray);
                offset = offset + contentLength;
                // 附件列表
                const attachment = [];
                let attachmentLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--attachmentLength >= 0) {
                    // 物品ID
                    const itemId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const number = view.getUint16(offset, false);
                    offset = offset + 2;
                    // add
                    attachment.push({itemId, number});
                }
                // add
                list.push({mailId, receiveTime, expireTime, readTime, receiveAttachmentTime, title, content, attachment});
            }
            return {list};
        }
        case 11402: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 11403: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 11404: {
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