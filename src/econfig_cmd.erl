%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc Defines command handler behaviour.
%%% In addition to the callbacks defined in this modules, commands should define
%%% the following attributes:
%%% * cmd_name: name of the command (atom)
%%% * cmd_desc: short description of the command (string)
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd).

%% Utils for commands
-export([usage/2,
		 usage/4]).

-callback usage() -> ok.
-callback run(State :: econfig_state:t(), Opts :: [proplists:property()]) -> ok.

usage(Cmd, Spec) ->
	getopt:usage(Spec, "econfig " ++ atom_to_list(Cmd)).

usage(Cmd, Spec, Help, Lines) ->
	getopt:usage(Spec, "econfig " ++ atom_to_list(Cmd), Help, Lines).
