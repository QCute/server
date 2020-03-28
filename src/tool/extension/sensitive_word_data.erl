-module(sensitive_word_data).
-compile(nowarn_export_all).
-compile(export_all).

-spec word(binary()) -> boolean().
word(<<>>) -> true;
word(_) -> false.
