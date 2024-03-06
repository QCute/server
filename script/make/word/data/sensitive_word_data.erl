-module(sensitive_word_data).
-export([word/1]).

-spec word(Word :: binary()) -> boolean().
word(<<>>) -> true;
word(_) -> false.

