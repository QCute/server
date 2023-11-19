require("./ProtocolDefine")

Reader = {length = 0, buffer = {"", ""}}

--- This function returns `self`.
--- @param buffer string
--- @return self
function Reader:appendData(buffer)
    self.buffer[2] = buffer or ""
    buffer = table.concat(self.buffer)
    self.buffer[1] = buffer
    self.buffer[2] = ""
    self.length = string.len(buffer)
    return self
end

--- This function returns `table|nil`.
--- @return table|nil packet
function Reader:read()
    -- @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
    if self.length >= 4 then
        local packetLength = string.unpack(">I2", self.buffer[1])
        if self.length >= 4 + packetLength then
            local protocol = string.unpack(">I2", self.buffer[1], 3)
            local packetData = string.sub(self.buffer[1], 5, 5 + packetLength)
            -- save
            self.buffer[1] = string.sub(self.buffer[1], 5 + packetLength)
            local meta = ProtocolDefine.getRead(protocol)
            local data = read(meta, 1, packetData)
            return { protocol = protocol, data = data }
        end
    end
end

function read(meta, offset, buffer)
    local type = meta["type"]
    local explain = meta["explain"]
    if type == "binary" then
        return string.unpack("c" .. explain, buffer, offset), offset + explain
    elseif type == "bool" then
        return string.unpack(">I1", buffer, offset) ~= 0, offset + 1
    elseif type == "u8" then
        return string.unpack(">I1", buffer, offset), offset + 1
    elseif type == "u16" then
        return string.unpack(">I2", buffer, offset), offset + 2
    elseif type == "u32" then
        return string.unpack(">I4", buffer, offset), offset + 4
    elseif type == "u64" then
        return string.unpack(">I8", buffer, offset), offset + 8
    elseif type == "i8" then
        return string.unpack(">i1", buffer, offset), offset + 1
    elseif type == "i16" then
        return string.unpack(">i2", buffer, offset), offset + 2
    elseif type == "i32" then
        return string.unpack(">i4", buffer, offset), offset + 4
    elseif type == "i64" then
        return string.unpack(">i8", buffer, offset), offset + 8
    elseif type == "f32" then
        return string.unpack(">f", buffer, offset), offset + 4
    elseif type == "f64" then
        return string.unpack(">d", buffer, offset), offset + 8
    elseif type == "str" then
        local data = string.unpack(">s2", buffer, offset)
        return data, offset + 2 + string.len(data)
    elseif type == "bst" then
        local data = string.unpack(">s2", buffer, offset)
        return data, offset + 2 + string.len(data)
    elseif type == "ast" then
        local data = string.unpack(">s2", buffer, offset)
        return data, offset + 2 + string.len(data)
    elseif type == "list" and key == nil then
        local listTable = {}
        local listLength = string.unpack(">I2", buffer, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- list only one explain
            local data, bytes = read(explain[1], offset, buffer)
            listTable[listIndex] = data
            offset = bytes
        end
        return listTable, offset
    elseif type == "list" then
        local listTable = {}
        local mapLength = string.unpack(">I2", buffer, offset)
        offset = offset + 2
        for _ = 1, mapLength do
            -- key list only one explain
            local data, bytes = read(explain[1], offset, buffer)
            listTable[data[key]] = data
            offset = bytes
        end
        return listTable, offset
    elseif type == "map" then
        local mapTable = {}
        for _, sub in pairs(explain) do
            local data, bytes = read(sub, offset, buffer)
            mapTable[sub["name"]] = data
            offset = bytes
        end
        return mapTable, offset
    else
        error(string.format("unknown type: ", meta["type"]))
    end
end
