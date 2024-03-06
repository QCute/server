## sql script 使用

* 配置  
```erl
sql() ->
    [
        #{
            file => "",                               %% 文件(相对路径)
            sql => [                                  %% SQL列表
            ]
        }
    ].
```

## 例子  

####  新增  

1. 基本新增
```erl
%% INSERT INTO `table` (`key`, `value`) VALUES (?, ?)
#{
    insert => [],
    into => table,
    as => new
}
```

2. 指定列
```erl
%% INSERT INTO `table` (`value`) VALUES (?)
#{
    insert => value,
    into => table,
    as => new
}
```

3. 指定多个列
```erl
%% INSERT INTO `table` (`key`, `value`) VALUES (?, ?)
#{
    insert => [key, value],
    into => table,
    as => new
}
```

4. 添加排除字段
```erl
%% INSERT INTO `table` (`value`) VALUES (?)
#{
    insert => [],
    except => key,
    into => table,
    as => new
}
```

5. 新增时, 主键/唯一索引冲突时, 选择忽略
```erl
%% INSERT IGNORE INTO `table` (`key`, `value`) VALUES (?, ?)
#{
    insert => [],
    duplicate => ignore,
    into => table,
    as => new
}
```

6. 新增时, 主键/唯一索引冲突时, 选择更新
```erl
%% INSERT INTO `table` (`key`, `value`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)
#{
    insert => [],
    duplicate => update,
    into => table,
    as => new
}
```

####  查询  

1. 基本查询
```erl
%% SELECT `key`, `value` FROM `table` WHERE `key` = ?
#{
    select => [],
    from => table,
    by => key,
    as => get
}
```

2. 指定列
```erl
%% SELECT `value` FROM `table` WHERE `key` = ?
#{
    select => value,
    from => table,
    by => key,
    as => get
}
```

3. 指定多个列
```erl
%% SELECT `key`, `value` FROM `table` WHERE `key` = ?
#{
    select => [key, value],
    from => table,
    by => key,
    as => get
}
```

4. 添加排除字段
```erl
%% SELECT `value` FROM `table` WHERE `key` = ?
#{
    select => [],
    except => key,
    from => table,
    by => key,
    as => get
}
```

5. 使用聚合窗口函数
```erl
%% SELECT MIN(`value`) AS `value` FROM `table` WHERE `key` = ?
#{
    select => #{
        value => min(value)
    },
    from => table,
    by => key,
    as => get
}
```

6. 使用原生SQL
```erl
%% SELECT 1 AS `value` FROM `table` WHERE `key` = ?
#{
    select => #{
        value => raw("1")
    },
    from => table,
    by => key,
    as => get
}
```

7. 使用多个筛选条件
```erl
%% SELECT `key`, `value` FROM `table` WHERE `key` = ? AND `value` = ?
#{
    select => [],
    from => table,
    by => [key, value],
    as => get
}
```

8. 使用其他比较方式
```erl
%% SELECT `key`, `value` FROM `table` WHERE `key` > ? AND `value` IN ?
#{
    select => [],
    from => table,
    by => #{
        key => '>',
        value => in
    },
    as => get
}
```

9. 使用预设值
```erl
%% SELECT `key`, `value` FROM `table` WHERE `key` = ? AND `value` = 6
#{
    select => [],
    from => table,
    by => #{
        key => param(),
        value => 6
    },
    as => get
}

%% SELECT `key`, `value` FROM `table` WHERE `key` > ? AND `value` < ? AND `value` = 7 AND `value` > 8
#{
    select => [],
    from => table,
    by => #{
        key => '>',
        value => #{
            between => {1, b},
            '<' => param(),
            '=' => 7,
            '>' => 8
        }
    },
    as => get
}
```

10. 使用 [group_by]()
```erl
%% SELECT `key`, `value` FROM `table` GROUP BY `key`
#{
    select => [],
    from => table,
    group_by => key,
    as => get
}
```

11. 使用 [having]()
```erl
%% SELECT `key`, `value` FROM `table` GROUP BY `key` HAVING `key` < ? AND `key` > 8
#{
    select => [],
    from => table,
    group_by => key,
    having => #{
        key => #{
            '<' => param(),
            '=' => 7,
            '>' => 8
        }
    },
    as => get
}
```

12. 使用 [order_by]()
```erl
%% SELECT `key`, ` FROM `table` GROUP BY `key` ORDER BY `valu`
#{
    select => [],
    from => table,
    group_by => key,
    order_by => value,
    as => get
}
```

13. 使用 [limit]()
```erl
%% SELECT `key`, `value` FROM `table` LIMIT 5
#{
    select => [],
    from => table,
    limit => 5,
    as => get
}
```

14. 使用 [offset]()
```erl
%% SELECT `key`, `value` FROM `table` LIMIT 5 OFFSET 6
#{
    select => [],
    from => table,
    limit => 5,
    offset => 6,
    as => get
}
```

15. 使用 [join]() 和 [use]()
```erl
%% SELECT 
%%   `other`.`id, 
%%   `another`.`v` 
%% FROM `table` 
%% INNER JOIN `other` 
%%   ON `other`.`name` = `table`.`value` 
%%   AND `other`.`id` = `table`.`key`
%% INNER JOIN `another` 
%%   ON `another`.`v` = `table`.`value` 
%%   AND `another`.`K` = `table`.`key`
#{
    select => [],
    from => table,
    join => #{
        other => #{
            name => value,
            id => key
        },
        another => #{
            v => value
            k => key
        }
    }
    use => #{
        key => "other.id",
        value => "another.v"
    },
    as => get
}
```

####  更新

1. 基本更新
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? WHERE `key` = ?
#{
    update => [],
    into => table,
    by => key,
    as => save
}
```

2. 指定列
```erl
%% UPDATE `table` SET `value` = ? WHERE `key` = ?
#{
    update => value,
    into => table,
    by => key,
    as => save
}
```

3. 指定多个列
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? WHERE `key` = ?
#{
    update => [key, value],
    into => table,
    by => key,
    as => save
}
```

4. 添加排除字段
```erl
%% UPDATE `table` SET `value` = ? WHERE `key` = ?
#{
    update => [],
    except => key,
    into => table,
    by => key,
    as => save
}
```

5. 使用聚合窗口函数
```erl
%% UPDATE `table` SET `value` = MIN(`value`) WHERE `key` = ?
#{
    update => #{
        value => min(value)
    },
    into => table,
    by => key,
    as => save
}
```

6. 使用原生SQL
```erl
%% UPDATE `table` SET `value` = 1 WHERE `key` = ?
#{
    update => #{
        value => raw("1")
    },
    into => table,
    by => key,
    as => save
}
```

7. 使用多个筛选条件
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? WHERE `key` = ? AND `value` = ?
#{
    update => [],
    into => table,
    by => [key, value],
    as => save
}
```

8. 使用其他比较方式
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? WHERE `key` > ? AND `value` IN ?
#{
    update => [],
    into => table,
    by => #{
        key => '>',
        value => in
    },
    as => save
}
```

9. 使用预设值
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? WHERE `key` > ? AND `value` < ? AND `value` > 8
#{
    update => [],
    into => table,
    by => #{
        key => '>',
        value => #{
            '<' => param(),
            '=' => 7,
            '>' => 8
        }
    },
    as => save
}
```

10. 使用 [GROUP BY]()
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? GROUP BY `key`
#{
    update => [],
    into => table,
    group_by => key,
    as => save
}
```

11. 使用 [HAVING]()
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? GROUP BY `key` HAVING `key` < ? AND `key` > 8
#{
    update => [],
    into => table,
    group_by => key,
    having => #{
        key => #{
            '<' => param(),
            '=' => 7,
            '>' => 8
        }
    },
    as => save
}
```

12. 使用 [ORDER BY]()
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? GROUP BY `key` ORDER BY `value`
#{
    update => [],
    into => table,
    group_by => key,
    order_by => value,
    as => save
}
```

13. 使用 [LIMIT]()
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? LIMIT 5
#{
    update => [],
    into => table,
    limit => 5,
    as => save
}
```

14. 使用 [OFFSET]()
```erl
%% UPDATE `table` SET `key` = ?, `value` = ? LIMIT 5 OFFSET 6
#{
    update => [],
    into => table,
    limit => 5,
    offset => 6,
    as => save
}
```

15. 使用 [JOIN]() 和 [USE]()
```erl
%% UPDATE `table` SET 
%%    `other`.`id = ?, 
%%    `another`.`v` = ? 
%% INNER JOIN `other` 
%%   ON `other`.`name` = `table`.`value` 
%%   AND `other`.`id` = `table`.`key`
%% INNER JOIN `another` 
%%   ON `another`.`v` = `table`.`value` 
%%   AND `another`.`K` = `table`.`key`
#{
    update => [],
    from => table,
    join => #{
        other => #{
            name => value,
            id => key
        },
        another => #{
            v => value
            k => key
        }
    }
    use => #{
        key => "other.id",
        value => "another.v"
    },
    as => save
}
```

####  删除

1. 基本删除

```erl
%% DELETE FROM `table` WHERE `key` = ?
#{
    delete => [],
    from => table,
    by => key,
    as => del
}
```

2. 指定列对删除无效
```erl
%% DELETE FROM `table` WHERE `key` = ?
#{
    delete => value,
    from => table,
    by => key,
    as => del
}
```

3. 指定多个列对删除无效
```erl
%% DELETE FROM `table` WHERE `key` = ?
#{
    delete => [key, value],
    from => table,
    by => key,
    as => del
}
```

4. 排除字段对删除无效
```erl
%% DELETE FROM `table` WHERE `key` = ?
#{
    delete => [],
    except => key,
    from => table,
    by => key,
    as => del
}
```

5. 使用聚合窗口函数
```erl
%% DELETE FROM `table` WHERE `key` = MIN(`key`)
#{
    delete => [],
    from => table,
    by => #{
        key => #{
            '=' => min(key)
        }
    },
    as => del
}
```

6. 使用原生SQL
```erl
%% DELETE FROM `table` WHERE `key` = 1
#{
    delete => [],
    from => table,
    by => #{
        key => #{
            '=' => raw("1")
        }
    },
    as => del
}
```

7. 使用多个筛选条件
```erl
%% DELETE FROM `table` WHERE `key` = ? AND `value` = ?
#{
    delete => [],
    from => table,
    by => [key, value],
    as => del
}
```

8. 使用其他比较方式
```erl
%% DELETE FROM `table` WHERE `key` > ? AND `value` IN ?
#{
    delete => [],
    from => table,
    by => #{
        key => '>',
        value => in
    },
    as => del
}
```

9. 使用预设值
```erl
%% DELETE FROM `table` WHERE `key` > ? AND `value` < ? AND `value` > 8
#{
    delete => [],
    from => table,
    by => #{
        key => '>',
        value => #{
            '<' => param(),
            '=' => 7,
            '>' => 8
        }
    },
    as => del
}
```

10. 使用 [GROUP BY]()
```erl
%% DELETE FROM `table` GROUP BY `key`
#{
    delete => [],
    from => table,
    group_by => key,
    as => del
}
```

11. 使用 [HAVING]()
```erl
%% DELETE FROM `table` GROUP BY `key` HAVING `key` < ? AND `key` > 8
#{
    delete => [],
    from => table,
    group_by => key,
    having => #{
        key => #{
            '<' => param(),
            '=' => 7,
            '>' => 8
        }
    },
    as => del
}
```

12. 使用 [ORDER BY]()
```erl
%% DELETE FROM `table` GROUP BY `key` ORDER BY `value`
#{
    delete => [],
    from => table,
    group_by => key,
    order_by => value,
    as => del
}
```

13. 使用 [LIMIT]()
```erl
%% DELETE FROM `table` LIMIT 5
#{
    delete => [],
    from => table,
    limit => 5,
    as => del
}
```

14. 使用 [OFFSET]()
```erl
%% DELETE FROM `table` LIMIT 5 OFFSET 6
#{
    delete => [],
    from => table,
    limit => 5,
    offset => 6,
    as => del
}
```

15. 使用 [JOIN]() 删除关联行, [use]()对删除无效
```erl
%% DELETE FROM `table` 
%% INNER JOIN `other` 
%%   ON `other`.`name` = `table`.`value` 
%%   AND `other`.`id` = `table`.`key`
%% INNER JOIN `another` 
%%   ON `another`.`v` = `table`.`value` 
%%   AND `another`.`K` = `table`.`key`
#{
    update => [],
    from => table,
    join => #{
        other => #{
            name => value,
            id => key
        },
        another => #{
            v => value
            k => key
        }
    }
    use => #{
        key => "other.id",
        value => "another.v"
    },
    as => save
}
```

## 清空

```erl
%% TRUNCATE TABLE `table`
#{
    truncate => table,
    as => truncate
}
```

## 生成  
```shell
maker sql 文件名(不含扩展名)  
```

## SQL释义  

| Key                           | Operation                     | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| insert                        | INSERT                        | 新增
| select                        | SELECT                        | 查询
| update                        | UPDATE                        | 更新
| delete                        | DELETE                        | 删除
| truncate                      | TRUNCATE                      | 清空
| except                        | EXCEPT Value                  | 排除字段
| filter                        |                               | 使用字段过滤未修改的行
| duplicate                     | DUPLICATE KEY                 | 主键/唯一索引冲突时操作
| into                          | INSERT/UPDATE TABLE NAME      | 新增/更新时使用, 指定的表名
| from                          | SELECT/DELETE TABLE NAME      | 查询/删除时使用, 指定的表名
| join                          | SET JOIN TABLE AND Value      | 设置联合查询的外表与连接字段
| use                           | REPLACE WITH JOIN Value       | 使用外表的字段替换主表字段
| by                            | WHERE                         | 查询条件
| group_by                      | GROUP BY                      | 组合条件
| having                        | HAVING                        | 组合后查询条件
| order_by                      | ORDER BY                      | 排序条件
| limit                         | LIMIT                         | 限制条数
| offset                        | OFFSET                        | 跳过条数
| return                        | RETURNING                     | 返回
| as                            |                               | 函数名字

* [insert]()/[select]()/[update]() and [except]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| value                         | \`value\`                     | 单个
| [v1, v2]                      | \`v1\`, \`v2\`                | 多个

* [insert]() => #{ }

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| #{k1 => raw("my sql")}        | VALUES(my sql)                | 原生SQL

* [select]() => #{ }

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| #{k1 => raw("my sql")}        | my sql AS \`k1\`              | 原生SQL

* [update]() => #{ }

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| #{k1 => raw("my sql")}        | SET \`k1\` = my sql           | 原生SQL

* [duplicate]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| update                        | ON DUPLICATE KEY UPDATE       | 更新
| ignore                        | ON DUPLICATE KEY IGNORE       | 忽略

* [join]() => #{ table => #{ foreign => local } }

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| t => #{f => l}                | JOIN \`t\` ON \`f\` = \`l\`   | 默认只支持交叉连接 - [INNER JOIN]()
| t => #{c => 1}                | JOIN \`t\` ON \`f\` = 1       | 使用字面值
| t => #{o => #{'LIKE' => 2}}   | JOIN \`t\` ON \`f\` LIKE 2    | 使用字面值
| t => #{o => #{in => param()}} | JOIN \`t\` ON \`f\` IN (?)    | 使用参数

* [use]() => #{ }

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| local => "table.foreign"      | \`table\`.\`foreign\`         | 使用连接表字段替换主表字段

* [by]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| key                           | \`key\` = ?                   | 单个
| [k1, k2]                      | \`k1\` = ? AND \`k2\` = ?     | 多个

* [by]() => #{ } - 使用其他比较方式

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => '='                     | \`k1\` = ?                    | 完全匹配
| k2 => '>'                     | \`k2\` > ?                    | 区间
| k3 => in                      | \`k3\` IN (?, ?, ?...)        | 查找多个
| k4 => 1                       | \`k4\` = 1                    | 使用字面值
| k5 => "2"                     | \`k5\` = '2'                  | 使用字面值
| k6 => #{like => param()}      | \`k4\` LIKE ?                 | 使用多个查找方式, 例如LIKE
| k7 => #{between => {3, 4}}    | \`k5\` BETWEEN ? AND ?        | 使用多个查找方式, 例如BETWEEN
| k8 => #{not_in => [5, "6"]}   | \`k6\` NOT IN (5, '6')        | 使用预设值, 例如NOT IN

* [group_by]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| type                          | GROUP BY \`type\`             | 单个
| [type, group]                 | GROUP BY \`type\`, \`group\`  | 多个

* [having]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| key                           | \`key\` = ?                   | 单个
| [k1, k2]                      | \`k1\` = ? AND \`k2\` = ?     | 多个

* [having]() => #{ } - 使用其他比较方式

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => '='                     | \`k1\` = ?                    | 完全匹配
| k2 => '>'                     | \`k2\` > ?                    | 区间
| k3 => in                      | \`k3\` IN (?, ?, ?...)        | 查找多个
| k4 => 1                       | \`k4\` = 1                    | 使用字面值
| k5 => "2"                     | \`k5\` = '2'                  | 使用字面值
| k6 => #{like => param()}      | \`k4\` LIKE ?                 | 使用多个查找方式, 例如LIKE
| k7 => #{between => {3, 4}}    | \`k5\` BETWEEN ? AND ?        | 使用多个查找方式, 例如BETWEEN
| k8 => #{not_in => [5, "6"]}   | \`k6\` NOT IN (5, '6')        | 使用预设值, 例如NOT IN

* [order_by]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| sort                          | ORDER BY \`sort\`             | 默认升序
| [k1, k2]                      | ORDER BY \`k1\`, \`k2\`       | 默认升序

* [order_by]() => #{} - 使用其他排序方式

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => asc                     | \`k1\` ASC                    | 指定升序
| k2 => desc                    | \`k2\` DESC                   | 指定降序

* [limit]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| 5                             | LIMIT 5                       | 最多5条

* [offset]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | ----------------------------
| 5                             | OFFSET 5                      | 跳过5条, 从第6条开始

* [return]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| key                           | \`key\`                       | 单个
| [k1, k2]                      | \`k1\`, \`k2\`                | 多个

* [return]() => #{ } - 使用其他返回方式

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => 1                       | 1 AS \`k1\`                   | 使用预设值
| k2 => max(k2)                 | MAX(\`k2\`) AS \`k2\`         | 使用聚合窗口函数
| k3 => raw("my sql")           | my sql AS \`k3\`              | 使用原生SQL

## 规则  

* 新增
    * 参数
        * 单个记录
        * 记录列表
    * 影响列
        * 不包含VIRTUAL字段
    * 返回
        * 自增值
        * 受影响的行数

* 查询
    * 参数
        * SQL条件参数
    * 影响列
        * tiny/small/int/big 转换为 non_neg_integer() 或者 integer()
        * decimal 转换为 float()
        * char 转换为 binary()
        * varchar 转换为 term()
    * 返回
        * 查询主键/唯一索引时: 返回单个记录或undefined
        * 其他: 返回记录列表

* 更新
    * 参数
        * SQL条件参数和记录
    * 影响列
        * 不包含VIRTUAL字段
    * 返回
        * 受影响的行数

* 删除
    * 参数
        * SQL条件参数
    * 影响列
        * 删除整行数据
    * 返回
        * 受影响的行数
