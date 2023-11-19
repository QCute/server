--- @class CheatQueryRequest
--- @field protocol number 60001
--- @field data {
--- }

--- @class CheatQueryRequest
--- @field protocol number 60001
--- @field data {
---     description: string,                                                                        -- 描述
---     command: string,                                                                            -- 命令
--- }[]

--- @class CheatCheatRequest
--- @field protocol number 60002
--- @field data string

--- @class CheatCheatRequest
--- @field protocol number 60002
--- @field data string

CheatProtocol = {}

function CheatProtocol.encode(offset, protocol, data)
    if protocol == 60001 then
        local table = {}

        return table
    elseif protocol == 60002 then
        local table = {}
        -- 命令
        table[offset] = string.pack(">s2", data)
        offset = offset + 1
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function CheatProtocol.decode(offset, protocol, bytes)
    if protocol == 60001 then
        -- 命令列表
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- 描述
            local dataDataDescription = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataDescription)
            -- 命令
            local dataDataCommand = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataCommand)
            -- object
            local dataData = {description = dataDataDescription, command = dataDataCommand}
            data[dataIndex] = dataData
        end
        return {protocol = 60001, data = data}
    elseif protocol == 60002 then
        -- 结果
        local data = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(data)
        return {protocol = 60002, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end