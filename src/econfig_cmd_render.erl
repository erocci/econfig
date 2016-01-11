%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_render).

-include("econfig_log.hrl").

-behaviour(econfig_cmd).

-cmd_name(render).
-cmd_desc("Render a file using bbmustache for substituing variables").

%% econfig_cmd behaviour API
-export([run/2,
		 usage/0]).

-define(argspec, [
				  {help,      $h, "help",      undefined,           "Show this help"}
				 ]).

run(State, Args) ->
	case getopt:parse(?argspec, Args) of
		{ok, {Opts, [In]}} ->
			case proplists:get_bool(help, Opts) of
				true -> 
					usage(),
					erlang:halt(0);
				false ->
					S2 = econfig_state:load(State),
					render(In, S2, Opts)
			end;
		_ ->
			usage(),
			erlang:halt(1)
	end.

usage() ->
	econfig_cmd:usage(print, ?argspec, "/path/to/template", []),
	ok.

%%%
%%% Priv
%%%
render(In, State, _Opts) ->
	Out = econfig_config:render(In, econfig_state:config(State)),
	io:format(Out).
