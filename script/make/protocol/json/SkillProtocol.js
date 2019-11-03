let skillProtocol = {
    read : {
        11701 : [
            {name : "list", type : "list", comment : "技能列表", explain : [
                {name : "skillId", type : "u32", comment : "技能ID", explain : []},
                {name : "level", type : "u16", comment : "技能等级", explain : []}
            ]}
        ]
    },
    write : {
        11701 : []
    }
};