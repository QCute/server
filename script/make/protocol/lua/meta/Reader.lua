require("./ProtocolDefine")

Reader = {buffer = {"", ""}}

--- This function returns `table`.
--- @param buffer {content = string
--- @return table
function Reader:read(buffer)
    self.buffer[2] = buffer or ""
    local data = table.concat(self.buffer)
    self.buffer[1] = data
    self.buffer[2] = ""
    local length = string.len(data)
    -- @tag protocol content length 2 bytes(without header 4 byte), protocol 2 bytes
    if length >= 4 then
        local packageLength = string.unpack(">I2", data)
        if length >= 4 + packageLength then
            local protocol = string.unpack(">I2", data, 3)
            local packageData = string.sub(data, 5, 5 + packageLength)
            -- save
            self.buffer[1] = string.sub(data, 5 + packageLength)
            local meta = getReadProtocolDefine(protocol)
            local result = read(meta, 1, packageData)
            return { protocol = protocol, content = result.content }
        end
    end
end

function read(metadata, offset, data)
    local dataTable = {}
    local switch = {
        u8 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">I1", thisData, thisOffset), offset = thisOffset + 1}
        end,
        u16 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">I2", thisData, thisOffset), offset = thisOffset + 2}
        end,
        u32 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">I4", thisData, thisOffset), offset = thisOffset + 4}
        end,
        u64 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">I8", thisData, thisOffset), offset = thisOffset + 8}
        end,
        i8 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">i1", thisData, thisOffset), offset = thisOffset + 1}
        end,
        i16 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">i2", thisData, thisOffset), offset = thisOffset + 2}
        end,
        i32 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">i4", thisData, thisOffset), offset = thisOffset + 4}
        end,
        i64 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">i8", thisData, thisOffset), offset = thisOffset + 8}
        end,
        f32 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">f", thisData, thisOffset), offset = thisOffset + 4}
        end,
        f64 = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">d", thisData, thisOffset), offset = thisOffset + 8}
        end,
        bool = function(meta, explain, thisOffset, thisData)
            return {content = string.unpack(">I1", thisData, thisOffset) ~= 0, offset = thisOffset + 1}
        end,
        binary = function(meta, explain, thisOffset, thisData)
            return {content = string.sub(thisData, thisOffset, explain), offset = thisOffset + explain}
        end,
        str = function(meta, explain, thisOffset, thisData)
            local string = string.unpack(">s2", thisData, thisOffset)
            return {content = string, offset = thisOffset + 2 + string.len(string)}
        end,
        bst = function(meta, explain, thisOffset, thisData)
            local string = string.unpack(">s2", thisData, thisOffset)
            return {content = string, offset = thisOffset + 2 + string.len(string)}
        end,
        rst = function(meta, explain, thisOffset, thisData)
            local string = string.unpack(">s2", thisData, thisOffset)
            return {content = string, offset = thisOffset + 2 + string.len(string)}
        end,
        list = function(meta, explain, thisOffset, thisData)
            local listTable = {}
            local listLength = string.unpack(">I2", thisData, thisOffset)
            thisOffset = thisOffset + 2
            for listIndex = 1, listLength do
                local result = read(explain, thisOffset, thisData)
                listTable[listIndex] = result["content"]
                thisOffset = result["offset"]
            end

            return {content = listTable, offset = thisOffset}
        end,
        map = function(meta, explain, thisOffset, thisData)
            local mapTable = {}
            local key = meta["key"]
            local listLength = string.unpack(">I2", thisData, thisOffset)
            thisOffset = thisOffset + 2
            for listIndex = 1, listLength do
                local result = read(explain, thisOffset, thisData)
                local content = result["content"]
                print(key)
                mapTable[content[key]] = content
                thisOffset = result["offset"]
            end
            return {content = mapTable, offset = thisOffset}
        end,
    }
    for index = 1, #metadata do
        local meta = metadata[index]
        local name = meta["name"]
        local explain = meta["explain"]
        local method = switch[meta["type"]]
        if method then
            local result = method(meta, explain, offset, data)
            dataTable[name] = result["content"]
            offset = result["offset"]
        else
            error(string.format("unknown type: ", meta["type"]))
        end
    end
    return {content = dataTable, offset = offset}
end
