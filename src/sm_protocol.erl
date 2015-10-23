-module(sm_protocol).
-author("robertzhouxh").

-type format() :: 'text' | 'binary'.
-type data() :: any().

-callback supports_format(format())
  -> boolean().

-callback encode(data(), format())
  -> data().

-callback decode(data(), format())
  -> data().
