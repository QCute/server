require("./ProtocolDefine")

Writer = {}

--- This function returns `string`.
--- @param protocol number
--- @param data table
--- @return string
function Writer:write(protocol, data)
    local meta = getWriteProtocolDefine(protocol)
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
    local switch = {
        u8 = function(meta, explain, thisData)
            return string.pack(">I1", thisData)
        end,
        u16 = function(meta, explain, thisData)
            return string.pack(">I2", thisData)
        end,
        u32 = function(meta, explain, thisData)
            return string.pack(">I4", thisData)
        end,
        u64 = function(meta, explain, thisData)
            return string.pack(">I8", thisData)
        end,
        i8 = function(meta, explain, thisData)
            return string.pack(">i1", thisData)
        end,
        i16 = function(meta, explain, thisData)
            return string.pack(">i2", thisData)
        end,
        i32 = function(meta, explain, thisData)
            return string.pack(">i4", thisData)
        end,
        i64 = function(meta, explain, thisData)
            return string.pack(">i8", thisData)
        end,
        f32 = function(meta, explain, thisData)
            return string.pack(">f", thisData)
        end,
        f64 = function(meta, explain, thisData)
            return string.pack(">d", thisData)
        end,
        bool = function(meta, explain, thisData)
            return string.pack(">I1", thisData and 1 or 0)
        end,
        binary = function(meta, explain, thisData)
            return thisData
        end,
        str = function(meta, explain, thisData)
            return string.pack(">s2", thisData)
        end,
        bst = function(meta, explain, thisData)
            return string.pack(">s2", thisData)
        end,
        rst = function(meta, explain, thisData)
            return string.pack(">s2", thisData)
        end,
        list = function(meta, explain, thisData)
            local listTable = {}
            listTable[1] = string.pack(">I2", #thisData)
            for listIndex = 1, #thisData do
                listTable[listIndex + 1] = table.concat(write(explain, 1, thisData[listIndex]))
            end
            return table.concat(listTable)
        end,
        map = function(meta, explain, thisData)
            local listTable = {}
            local listIndex = 1
            listTable[1] = string.pack(">I2", #thisData)
            for _, item in pairs(thisData) do
                listTable[listIndex + 1] = table.concat(write(explain, 1, item))
                listIndex = listIndex + 1
            end
            return table.concat(listTable)
        end,
    }
    for index = 1, #metadata do
        local meta = metadata[index]
        local name = meta["name"]
        local explain = meta["explain"]
        local method = switch[meta["type"]]
        if method then
            dataTable[offset] = method(meta, explain, data[name])
            offset = offset + 1
        else
            error(string.format("unknown type: ", meta["type"]))
        end
    end
    return dataTable
end