-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

-ifdef(DEBUG).

-define(I(Fmt, Args), error_logger:info_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(I(Fmt),       ?I(Fmt, [])).

-define(W(Fmt, Args), error_logger:warning_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(W(Fmt),       ?W(Fmt, [])).

-define(SINFO(StateName), ?I("going to state '~w'", [StateName])).

-else.

-define(I(Fmt, Args), ok).
-define(I(Fmt),       ok).

-define(W(Fmt, Args), ok).
-define(W(Fmt),       ok).

-define(SINFO(StateName), ok).

-endif.

-define(E(Fmt, Args),  error_logger:error_msg("~s:~w: " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).
-define(E(Fmt),       ?E(Fmt, [])).

-endif.

