%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_log).

-include("econfig_log.hrl").

-export([start_link/0]).

-export([debug/1,
	 debug/2,
	 info/1,
	 info/2,
	 warn/1,
	 warn/2,
	 error/1,
	 error/2,
	 log/3]).

%% Internals
-export([init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

debug(Msg) -> log(?LVL_DEBUG, Msg ++ "~n", []).
debug(Msg, Data) -> log(?LVL_DEBUG, Msg, Data).

info(Msg) -> log(?LVL_INFO, Msg ++ "~n", []).
info(Msg, Data) -> log(?LVL_INFO, Msg, Data).

warn(Msg) -> log(?LVL_WARN, Msg ++ "~n", []).
warn(Msg, Data) -> log(?LVL_WARN, Msg, Data).

error(Msg) -> log(?LVL_ERROR, Msg ++ "~n", []).
error(Msg, Data) -> log(?LVL_ERROR, Msg, Data).

log(Lvl, Msg, Data) ->
    try ?MODULE ! {log, Lvl, Msg, Data}
    catch error:badarg -> log_tty(Lvl, Msg, Data)
    end.

%%%
%%% Priv
%%%
init() ->
    LogLevel = application:get_env(econfig, log, ?LVL_INFO),
    case application:get_env(econfig, caller, escript) of
	escript ->
	    loop_tty(LogLevel);
	rebar ->
	    loop_rebar(LogLevel);
	_ ->
	    loop_erts(LogLevel)
    end.


loop_erts(MaxLevel) ->
    receive
	{log, Lvl, Msg, Data} when Lvl =< MaxLevel ->
	    log_erts(Lvl, Msg, Data),
	    loop_erts(MaxLevel);
	{log, _, _, _} ->
	    loop_erts(MaxLevel)
    end.


log_erts(?LVL_DEBUG, Msg, Data) ->
    error_logger:info_msg(Msg, Data);
log_erts(?LVL_INFO, Msg, Data) ->
    error_logger:info_msg(Msg, Data);
log_erts(?LVL_WARN, Msg, Data) ->
    error_logger:warning_msg(Msg, Data);
log_erts(?LVL_ERROR, Msg, Data) ->
    error_logger:error_msg(Msg, Data).


loop_tty(MaxLevel) ->
    receive
	{log, Lvl, Msg, Data} when Lvl =< MaxLevel ->
	    log_tty(Lvl, Msg, Data),
	    loop_tty(MaxLevel);
	{log, _, _, _} ->
	    loop_tty(MaxLevel)
    end.


log_tty(?LVL_DEBUG, Msg, Data) ->
    io:format(standard_io, "D: " ++ Msg, Data);
log_tty(?LVL_INFO, Msg, Data) ->
    io:format(standard_io, "I: " ++ Msg, Data);
log_tty(?LVL_WARN, Msg, Data) ->
    io:format(standard_error, "W: " ++ Msg, Data);
log_tty(?LVL_ERROR, Msg, Data) ->
    io:format(standard_error, "E: " ++ Msg, Data).


loop_rebar(_) ->
    receive
	{log, Lvl, Msg, Data} ->
	    case Lvl of
		?LVL_DEBUG ->
		    rebar_log:log(debug, Msg, Data);
		?LVL_INFO ->
		    rebar_log:log(info, Msg, Data);
		?LVL_WARN ->
		    rebar_log:log(warn, Msg, Data);
		?LVL_ERROR ->
		    rebar_log:log(error, Msg, Data)
	    end,
	    loop_rebar([])
    end.
