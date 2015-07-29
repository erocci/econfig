-ifndef(econfig_log_hrl).
-define(econfig_log_hrl, true).

-define(LVL_DEBUG, 0).
-define(LVL_INFO, 1).
-define(LVL_WARN, 2).
-define(LVL_ERROR, 3).

-ifndef(debug).
-define(debug(Msg), econfig_log:log(?LVL_DEBUG, Msg ++ "~n", [])).
-define(debug(Msg, Data), econfig_log:log(?LVL_DEBUG, Msg, Data)).
-endif.

-ifndef(info).
-define(info(Msg), econfig_log:log(?LVL_INFO, Msg ++ "~n", [])).
-define(info(Msg, Data), econfig_log:log(?LVL_INFO, Msg, Data)).
-endif.

-ifndef(warn).
-define(warn(Msg), econfig_log:log(?LVL_WARN, Msg ++ "~n", [])).
-define(warn(Msg, Data), econfig_log:log(?LVL_WARN, Msg, Data)).
-endif.

-ifndef(error).
-define(error(Msg), econfig_log:log(?LVL_WARN, Msg ++ "~n", [])).
-define(error(Msg, Data), econfig_log:log(?LVL_ERROR, Msg, Data)).
-endif.

-endif.
