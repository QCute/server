function encodeRoleProtocol(offset, protocol, data)
    local switch = {

    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeRoleProtocol(offset, protocol, data)
    local switch = {
        [10101] = function()
            local offset = offset
            -- 角色ID
            local roleId = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 角色名
            local roleName = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(roleName)
            -- 性别
            local sex = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 职业
            local classes = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 等级
            local level = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 普通背包大小
            local itemSize = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 装备背包大小
            local bagSize = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- 仓库背包大小
            local storeSize = string.unpack(">I2", data, offset)
            offset = offset + 2
            return {roleId = roleId, roleName = roleName, sex = sex, classes = classes, level = level, itemSize = itemSize, bagSize = bagSize, storeSize = storeSize}
        end,
        [10102] = function()
            local offset = offset
            -- 金币
            local gold = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 银币
            local silver = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- 铜币
            local copper = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 经验
            local exp = string.unpack(">I8", data, offset)
            offset = offset + 8
            return {gold = gold, silver = silver, copper = copper, exp = exp}
        end,
        [10103] = function()
            local offset = offset
            -- 等级
            local vipLevel = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- 经验
            local exp = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- 过期时间
            local expireTime = string.unpack(">I4", data, offset)
            offset = offset + 4
            return {vipLevel = vipLevel, exp = exp, expireTime = expireTime}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end