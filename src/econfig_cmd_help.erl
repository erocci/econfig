%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_help).

-behaviour(econfig_cmd).

-cmd_name(help).
-cmd_desc("Describe a command and its options").

%% econfig_cmd behaviour API
-export([run/2]).

run(_State, _Opts) ->
	io:format("Usage: econfig help <"
			  ++ string:join(econfig_cli:cmd_names(), " | ")
			  ++ ">~n",
			 []),
    ok.
