open Core_replace

val ok_or_stderr : ('a, string) Result_.t -> ?line:int -> on_error:'a -> 'a
