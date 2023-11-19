## SQL目录说明

## 子目录

1. 子目录 alpha, beta, ... 是此版本变更的SQL
2. 子目录下alpha/test.sql, beta/boss.sql 是此版本此功能的SQL

## 文件规范

#### 数据库要求

1. 使用**MariaDB**和**InnoDB**引擎
2. 字符集**utf8mb4**和校对规则**utf8mb4_unicode_ci**(unicode为德/法/俄语等校验)  
2. **配置表**和**数据表**使用**Dynamic**行格式,**日志表**使用**Compressed**行格式  
3. **整数类型**tiny(3)/small(5)/int(10)/big(20)默认为0非空且无符号(unsigned)  
4. **字符串类型**char/varchar默认为空字符串且非空  

#### SQL语句放置顺序要求

1. 配置表  *_data  
2. 配置测试表  *_test_data  
3. 数据表  *  
4. 日志表  *_log  
5. 更改字段语句
6. 增加/修改数据语句
