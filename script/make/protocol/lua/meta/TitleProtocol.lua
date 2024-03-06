return {
    [11901] = {
        ["comment"] = "称号列表",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        },
        ["read"] = {
            {name = "data", type = "list", comment = "称号列表", explain = 
                {name = "title", type = "record", comment = "", explain = {
                    {name = "titleId", type = "u32", comment = "称号ID", explain = {}},
                    {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
                }}
            }
        }
    }
}