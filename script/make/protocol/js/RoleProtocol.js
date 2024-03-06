export default class RoleProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 10101: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 10102: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 10103: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 10101: {
                // 角色ID
                const roleId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 角色名
                const roleNameLength = view.getUint16(offset, false);
                offset = offset + 2;
                const roleNameArray = new Uint8Array(view.buffer.slice(offset, offset + roleNameLength));
                const roleName = textDecoder.decode(roleNameArray);
                offset = offset + roleNameLength;
                // 性别
                const sex = view.getUint8(offset, false);
                offset = offset + 1;
                // 职业
                const classes = view.getUint8(offset, false);
                offset = offset + 1;
                // 等级
                const level = view.getBigUint64(offset, false);
                offset = offset + 8;
                return {roleId, roleName, sex, classes, level};
            }
            case 10102: {
                // 金币
                const gold = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 银币
                const silver = view.getUint32(offset, false);
                offset = offset + 4;
                // 铜币
                const copper = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 经验
                const exp = view.getBigUint64(offset, false);
                offset = offset + 8;
                return {gold, silver, copper, exp};
            }
            case 10103: {
                // 等级
                const vipLevel = view.getUint8(offset, false);
                offset = offset + 1;
                // 经验
                const exp = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 过期时间
                const expireTime = view.getUint32(offset, false);
                offset = offset + 4;
                return {vipLevel, exp, expireTime};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}