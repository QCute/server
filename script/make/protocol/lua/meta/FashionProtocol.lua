return {
    [12001] = {
        ["comment"] = "时装列表",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        },
        ["read"] = {
            {name = "data", type = "list", comment = "时装列表", explain = 
                {name = "fashion", type = "record", comment = "", explain = {
                    {name = "fashionId", type = "u32", comment = "时装ID", explain = {}},
                    {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
                }}
            }
        }
    }
}