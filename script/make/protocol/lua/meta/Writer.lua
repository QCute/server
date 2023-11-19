require("./ProtocolDefine")

Writer = {}

--- This function returns `string`.
--- @param protocol number
--- @param data table
--- @return string
function Writer:write(protocol, data)
    local meta = ProtocolDefine.getWrite(protocol)
    local capacity = write(meta, 3, data)
    local length = string.len(capacity)
    local lengthString = string.pack(">I2", length)
    local protocolString = string.pack(">I2", protocol)
    return table.concat({lengthString, protocolString, capacity})
end

function write(meta, offset, data)
    local type = meta["type"]
    local explain = meta["explain"]
    local key = meta["key"]
    if type == "binary" then
        return data
    elseif type == "bool" then
        return string.pack(">I1", data and 1 or 0)
    elseif type == "u8" then
        return string.pack(">I1", data)
    elseif type == "u16" then
        return string.pack(">I2", data)
    elseif type == "u32" then
        return string.pack(">I4", data)
    elseif type == "u64" then
        return string.pack(">I8", data)
    elseif type == "i8" then
        return string.pack(">i1", data)
    elseif type == "i16" then
        return string.pack(">i2", data)
    elseif type == "i32" then
        return string.pack(">i4", data)
    elseif type == "i64" then
        return string.pack(">i8", data)
    elseif type == "f32" then
        return string.pack(">f", data)
    elseif type == "f64" then
        return string.pack(">d", data)
    elseif type == "str" then
        return string.pack(">s2", data)
    elseif type == "bst" then
        return string.pack(">s2", data)
    elseif type == "ast" then
        return string.pack(">s2", data)
    elseif type == "list" and key == nil then
        local listTable = {}
        listTable[1] = string.pack(">I2", #data)
        for listIndex = 1, #data do
            -- list only one explain
            listTable[listIndex + 1] = write(explain[1], 1, data[listIndex])
        end
        return table.concat(listTable)
    elseif type == "list" then
        local listTable = {}
        local listIndex = 1
        listTable[1] = string.pack(">I2", #data)
        for _, value in pairs(data) do
            -- key list only one explain
            listTable[listIndex + 1] = write(explain[1], 1, value)
            listIndex = listIndex + 1
        end
        return table.concat(listTable)
    elseif type == "map" then
        local mapTable = {}
        local mapIndex = 1
        mapTable[1] = string.pack(">I2", #data)
        for _, sub in pairs(explain) do
            mapTable[mapIndex] = write(sub, 1, data[sub["name"]])
            mapIndex = mapIndex + 1
        end
        return table.concat(mapTable)
    else
        error(string.format("unknown type: ", meta["type"]))
    end
end
