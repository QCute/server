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
            local result = read(meta, 1, packetData)
            return { protocol = protocol, data = result.data }
        end
    end
end

function read(meta, offset, buffer)
    local name = meta["name"]
    local type = meta["type"]
    local explain = meta["explain"]
    if type == "binary" then
        return string.sub(buffer, offset, explain), offset + explain
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
        return string.unpack(">s2", buffer, offset), offset + 2 + string.len(data[name])
    elseif type == "bst" then
        return string.unpack(">s2", buffer, offset), offset + 2 + string.len(data[name])
    elseif type == "rst" then
        return string.unpack(">s2", buffer, offset), offset + 2 + string.len(data[name])
    elseif type == "list" and key == nil then
        local listTable = {}
        local listLength = string.unpack(">I2", buffer, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            local result, bytes = read(explain[1], offset, buffer)
            listTable[listIndex] = result;
            offset = offset + bytes
        end
        return listTable
    elseif type == "list" then
        local mapTable = {}
        local key = meta["key"]
        local mapLength = string.unpack(">I2", buffer, offset)
        offset = offset + 2
        for mapIndex = 1, mapLength do
            local result, bytes = read(explain[1], offset, buffer)
            mapTable[data[key]] = result
            offset = offset + bytes
        end
        return mapTable
    elseif type == "map" then
        local mapTable = {}
        local mapIndex = 1
        mapTable[1] = string.pack(">I2", #data)
        for key, sub in pairs(explain) do
            mapTable[mapIndex] = read(sub, 1, data[sub["name"]]);
            mapIndex = mapIndex + 1;
        end
        return table.concat(mapTable)

        local mapTable = {}
        local key = meta["key"]
        local mapLength = string.unpack(">I2", buffer, offset), offset + 2
        for mapIndex = 1, mapLength do
            local result = read(explain, offset, buffer)
            local data = result["data"]
            mapTable[data[key]] = result["data"]
            offset = result["offset"]
        end
        return mapTable
    else
        error(string.format("unknown type: ", meta["type"]))
    end
end

end
