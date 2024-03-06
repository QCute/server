require("./ProtocolDefine")

Writer = {}

--- This function returns `string`.
--- @param protocol number
--- @param data table
--- @return string
function Writer:write(protocol, data)
    local meta = ProtocolDefine.getWrite(protocol)
    local dataTable = write(meta, 3, data)
    dataTable[1] = ""
    dataTable[2] = ""
    local length = string.len(table.concat(dataTable))
    dataTable[1] = string.pack(">I2", length)
    dataTable[2] = string.pack(">I2", protocol)
    return table.concat(dataTable)
end

function write(metadata, offset, data)
    local dataTable = {}
    for index = 1, #metadata do
        local meta = metadata[index]
        local name = meta["name"]
        local explain = meta["explain"]
        local type = meta["type"]
        if type == "binary" then
            dataTable[offset] = data[name]
        elseif type == "bool" then
            dataTable[offset] = string.pack(">I1", data[name] and 1 or 0)
        elseif type == "u8" then
            dataTable[offset] = string.pack(">I1", data[name])
        elseif type == "u16" then
            dataTable[offset] = string.pack(">I2", data[name])
        elseif type == "u32" then
            dataTable[offset] = string.pack(">I4", data[name])
        elseif type == "u64" then
            dataTable[offset] = string.pack(">I8", data[name])
        elseif type == "i8" then
            dataTable[offset] = string.pack(">i1", data[name])
        elseif type == "i16" then
            dataTable[offset] = string.pack(">i2", data[name])
        elseif type == "i32" then
            dataTable[offset] = string.pack(">i4", data[name])
        elseif type == "i64" then
            dataTable[offset] = string.pack(">i8", data[name])
        elseif type == "f32" then
            dataTable[offset] = string.pack(">f", data[name])
        elseif type == "f64" then
            dataTable[offset] = string.pack(">d", data[name])
        elseif type == "str" then
            dataTable[offset] = string.pack(">s2", data[name])
        elseif type == "bst" then
            dataTable[offset] = string.pack(">s2", data[name])
        elseif type == "rst" then
            dataTable[offset] = string.pack(">s2", data[name])
        elseif type == "list" then
            local listTable = {}
            local listData = data[name]
            listTable[1] = string.pack(">I2", #listData)
            for listIndex = 1, #listData do
                listTable[listIndex + 1] = table.concat(write(explain, 1, listData[listIndex]))
            end
            dataTable[offset] = table.concat(listTable)
        elseif type == "map" then
            local mapTable = {}
            local mapIndex = 1
            local mapData = data[name]
            mapTable[1] = string.pack(">I2", #mapData)
            for _, item in pairs(mapData) do
                mapTable[mapIndex + 1] = table.concat(write(explain, 1, item))
            end
            dataTable[offset] = table.concat(mapTable)
        else
            error(string.format("unknown type: ", meta["type"]))
        end
        offset = offset + 1
    end
    return dataTable
end
