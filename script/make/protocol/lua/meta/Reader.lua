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

--- This function returns `table`.
--- @return table
function Reader:read()
    -- @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
    if self.length >= 4 then
        local packetLength = string.unpack(">I2", self.buffer[1])
        if self.length >= 4 + packetLength then
            local protocol = string.unpack(">I2", self.buffer[1], 3)
            local packetData = string.sub(self.buffer[1], 5, 5 + packetLength)
            -- save
            self.buffer[1] = string.sub(self.buffer[1], 5 + packetLength)
            local meta = getReadProtocolDefine(protocol)
            local result = read(meta, 1, packetData)
            return { protocol = protocol, data = result.data }
        end
    end
end

function read(metadata, offset, buffer)
    local data = {}
    for index = 1, #metadata do
        local meta = metadata[index]
        local name = meta["name"]
        local explain = meta["explain"]
        local type = meta["type"]
        if type == "binary" then
            data[name] = string.sub(buffer, offset, explain)
            offset = offset + explain
        elseif type == "bool" then
            data[name] = string.unpack(">I1", buffer, offset) ~= 0
            offset = offset + 1
        elseif type == "u8" then
            data[name] = string.unpack(">I1", buffer, offset)
            offset = offset + 1
        elseif type == "u16" then
            data[name] = string.unpack(">I2", buffer, offset)
            offset = offset + 2
        elseif type == "u32" then
            data[name] = string.unpack(">I4", buffer, offset)
            offset = offset + 4
        elseif type == "u64" then
            data[name] = string.unpack(">I8", buffer, offset)
            offset = offset + 8
        elseif type == "i8" then
            data[name] = string.unpack(">i1", buffer, offset)
            offset = offset + 1
        elseif type == "i16" then
            data[name] = string.unpack(">i2", buffer, offset)
            offset = offset + 2
        elseif type == "i32" then
            data[name] = string.unpack(">i4", buffer, offset)
            offset = offset + 4
        elseif type == "i64" then
            data[name] = string.unpack(">i8", buffer, offset)
            offset = offset + 8
        elseif type == "f32" then
            data[name] = string.unpack(">f", buffer, offset)
            offset = offset + 4
        elseif type == "f64" then
            data[name] = string.unpack(">d", buffer, offset)
            offset = offset + 8
        elseif type == "str" then
            data[name] = string.unpack(">s2", buffer, offset)
            offset = offset + 2 + string.len(data[name])
        elseif type == "bst" then
            data[name] = string.unpack(">s2", buffer, offset)
            offset = offset + 2 + string.len(data[name])
        elseif type == "rst" then
            data[name] = string.unpack(">s2", buffer, offset)
            offset = offset + 2 + string.len(data[name])
        elseif type == "list" then
            local listTable = {}
            local listLength = string.unpack(">I2", buffer, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                local result = read(explain, offset, buffer)
                listTable[listIndex] = result["data"]
                offset = result["offset"]
            end
            data[name] = listTable
        elseif type == "map" then
            local mapTable = {}
            local key = meta["key"]
            local mapLength = string.unpack(">I2", buffer, offset)
            offset = offset + 2
            for mapIndex = 1, mapLength do
                local result = read(explain, offset, buffer)
                local data = result["data"]
                mapTable[data[key]] = result["data"]
                offset = result["offset"]
            end
            data[name] = mapTable
        else
            error(string.format("unknown type: ", meta["type"]))
        end
    end
    return {data = data, offset = offset}
end
