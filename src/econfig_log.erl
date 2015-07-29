%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_log).

-include("econfig_log.hrl").

-export([debug/1,
	 debug/2,
	 info/1,
	 info/2,
	 warn/1,
	 warn/2,
	 error/1,
	 error/2]).

-export([log/3]).

debug(Msg) -> log(?LVL_DEBUG, Msg ++ "~n", []).
debug(Msg, Data) -> log(?LVL_DEBUG, Msg, Data).

info(Msg) -> log(?LVL_INFO, Msg ++ "~n", []).
info(Msg, Data) -> log(?LVL_INFO, Msg, Data).

warn(Msg) -> log(?LVL_WARN, Msg ++ "~n", []).
warn(Msg, Data) -> log(?LVL_WARN, Msg, Data).

error(Msg) -> log(?LVL_ERROR, Msg ++ "~n", []).
error(Msg, Data) -> log(?LVL_ERROR, Msg, Data).

log(Lvl, Msg, Data) ->
    case application:get_env(econfig, escript, false) of
	false -> log_erts(Lvl, Msg, Data);
	true -> log_tty(Lvl, Msg, Data)
    end.

%%%
%%% Priv
%%%
log_erts(?LVL_DEBUG, Msg, Data) ->
    error_logger:info_msg(Msg, Data);
log_erts(?LVL_INFO, Msg, Data) ->
    error_logger:info_msg(Msg, Data);
log_erts(?LVL_WARN, Msg, Data) ->
    error_logger:warning_msg(Msg, Data);
log_erts(?LVL_ERROR, Msg, Data) ->
    error_logger:error_msg(Msg, Data).


log_tty(?LVL_DEBUG, Msg, Data) ->
    io:format(standard_io, "D: " ++ Msg, Data);
log_tty(?LVL_INFO, Msg, Data) ->
    io:format(standard_io, "I: " ++ Msg, Data);
log_tty(?LVL_WARN, Msg, Data) ->
    io:format(standard_error, "W: " ++ Msg, Data);
log_tty(?LVL_ERROR, Msg, Data) ->
    io:format(standard_error, "E: " ++ Msg, Data).
