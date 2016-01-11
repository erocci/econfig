%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_configure).

-include("econfig_log.hrl").

-behaviour(econfig_cmd).

%% econfig_cmd behaviour API
-export([run/2,
		 usage/0]).

-cmd_name(configure).
-cmd_desc("Configure an application").

-define(frontends, [defconfig,tty]).
-define(frontends_str, 
		io_lib:format("(~s)", [string:join([ atom_to_list(F) || F <- ?frontends], "|")])).
-define(argspec, [
				  {overwrite, $w, "overwrite", {boolean, false}, "Overwrite existing config"}
				 ,{frontend,  $f, "frontend",  {string, "tty"},  "User frontend " ++ ?frontends_str}
				 ,{help,      $h, "help",      undefined,        "Show this help"}
				 ]).

run(State, Args) ->
	case getopt:parse(?argspec, Args) of
		{ok, {Opts, Models}} ->
			case proplists:get_bool(help, Opts) of
				true -> 
					usage(),
					erlang:halt(0);
				false ->
					init(Models, State, Opts)
			end;
		_ ->
			usage(),
			erlang:halt(1)
	end.

usage() ->
	econfig_cmd:usage(configure, ?argspec, "", []).

%%%
%%% Priv
%%%
init(Models, State, Opts) ->
	case proplists:get_value(overwrite, Opts, false) of
		false ->
			init_config(Models, State, Opts);
		true ->
			init_models(Models, State, Opts)
	end.

init_config(Models, S, Opts) ->
	case econfig_state:load(S) of
		{error, _} = Err ->
			Err;
		S2 ->
			init_models(Models, S2, Opts)
    end.

init_models([], State, Opts) ->
    ?debug("No model specified: load from current dir", []),
    Cwd = econfig_state:basedir(State),
    init_models([Cwd], State, Opts);
init_models(ModelsDef, State, Opts) ->
    case econfig_state:parse_models(parse_models_list(ModelsDef, []), State) of
		{error, _} = Err ->
			Err;
		S2 ->
			run2(S2, Opts)
    end.

parse_models_list([], Acc) ->
    lists:reverse(Acc);
parse_models_list([ Str | Tail], Acc) ->
    case string:tokens(Str, ":") of
		[Dir] ->
			App = filename:basename(Dir),
			parse_models_list(Tail, [{list_to_atom(App), Dir} | Acc]);
		[AppStr, Dir] ->
			parse_models_list(Tail, [{list_to_atom(AppStr), Dir} | Acc]);
		_ ->
			throw({model_list_parse_error, Str})
    end.

frontend(Frontend) -> 
    case lists:member(Frontend, [ atom_to_list(F) || F <- ?frontends]) of
		true ->
			list_to_atom(Frontend);
		false ->
			throw({invalid_frontend, Frontend})
    end.

run2(State, Opts) ->
	Frontend = frontend(proplists:get_value(frontend, Opts, defconfig)),
    case econfig_state:configure(Frontend, State) of
		{error, Err} ->
			econfig_cli:handle_error(Err);
		S2 ->
			econfig_state:store(S2)
    end.
