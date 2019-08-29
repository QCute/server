local mapProtocol = {
    ["read"] = {
        [20001] = {},
        [20002] = {
            {name = "x", type = "u16", comment = "X坐标", explain = {}},
            {name = "y", type = "u16", comment = "Y坐标", explain = {}}
        }
    },
    ["write"] = {
        [20001] = {
            {name = "mapId", type = "u32", comment = "地图ID", explain = {}},
            {name = "x", type = "u16", comment = "X坐标", explain = {}},
            {name = "y", type = "u16", comment = "Y坐标", explain = {}}
        },
        [20002] = {
            {name = "x", type = "u16", comment = "X坐标", explain = {}},
            {name = "y", type = "u16", comment = "Y坐标", explain = {}}
        }
    }
}