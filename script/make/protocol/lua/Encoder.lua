require("./ProtocolRouter")

Encoder = {}

--- This function returns `string`.
--- @param protocol number
--- @param data table
--- @return string
function Encoder:encode(protocol, data)
    local dataTable = encodeProtocol(3, protocol, data)
    dataTable[1] = ""
    dataTable[2] = ""
    -- @tag protocol content length 2 bytes(without header 4 byte), protocol 2 bytes
    local length = string.len(table.concat(dataTable))
    dataTable[1] = string.pack(">I2", length)
    dataTable[2] = string.pack(">I2", protocol)
    return table.concat(dataTable)
end
