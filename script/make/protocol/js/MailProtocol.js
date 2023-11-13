export default class MailProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11401: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11402: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 邮件ID
                view.setBigUint64(offset, data, false);
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
                view.setBigUint64(offset, data, false);
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
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11401: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
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
                        // 
                        // 物品ID
                        const attachmentItemId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const attachmentNumber = view.getUint16(offset, false);
                        offset = offset + 2;
                        // object
                        const attachmentItem = {"itemId": attachmentItemId, "number": attachmentNumber};
                        // add
                        attachment.push(attachmentItem);
                    }
                    // object
                    const mail = {"mailId": mailId, "receiveTime": receiveTime, "expireTime": expireTime, "readTime": readTime, "receiveAttachmentTime": receiveAttachmentTime, "title": title, "content": content, "attachment": attachment};
                    // add
                    data.push(mail);
                }
                return data;
            }
            case 11402: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 11403: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 11404: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}