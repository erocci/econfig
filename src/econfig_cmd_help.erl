%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_help).

-behaviour(econfig_cmd).

-include("econfig_log.hrl").

-cmd_name(help).
-cmd_desc("Describe a command and its options").

%% econfig_cmd behaviour API
-export([run/2,
		 usage/0]).

run(_, []) ->
	usage();
run(_, [Cmd]) ->
	case econfig_cli:cmd_mod(Cmd) of
		undefined ->
			?error("Invalid command: ~s", [Cmd]);
		Mod ->
			Mod:usage()
	end.

usage() ->
	Commands = lists:filter(fun ("help") -> false; (_) -> true end, econfig_cli:cmd_names()),
	econfig_cmd:usage(help, [], "<" ++ string:join(Commands, " | ") ++ ">", []).
