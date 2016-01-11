%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_print).

-behaviour(econfig_cmd).

-cmd_name(print).
-cmd_desc("Print current configuration").

%% econfig_cmd behaviour API
-export([run/2,
		 usage/0]).

-define(argspec, [
				  {help,      $h, "help",      undefined,        "Show this help"}
				 ]).

run(State, _Opts) ->
    econfig_config:pp(econfig_state:config(State)),
    ok.

usage() ->
	econfig_cmd:usage(print, ?argspec),
	ok.
