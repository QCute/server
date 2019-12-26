-module(error_code_data).
-compile(nowarn_export_all).
-compile(export_all).


get(10001, asn1) ->
    <<"未知字符"/utf8>>;
get(10001, duplicate) ->
    <<"重复登录"/utf8>>;
get(10001, failed) ->
    <<"失败"/utf8>>;
get(10001, length) ->
    <<"长度不对"/utf8>>;
get(10001, sensitive) ->
    <<"包含敏感词"/utf8>>;
get(_, _) ->
    <<>>.


