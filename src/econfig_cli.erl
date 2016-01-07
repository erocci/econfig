%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cli).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([run/1]).

-type econfig_cmd() :: configure | print.
-type econfig_opts() :: {frontend, tty}.

-define(frontends_str, 
		io_lib:format("(~s)", [string:join([ atom_to_list(F) || F <- ?frontends], "|")])).

-define(argspec, [
				  {overwrite, $o, "overwrite", {boolean, false}, "Overwrite existing config"},
				  {frontend,  $f, "frontend",  {string, "tty"},  "User frontend " ++ ?frontends_str},
				  {cwd,       $C, "dir",       string,           "Change working directory"},
				  {verbose,   $v, "verbose",   integer,          "Verbose (multiple times increase verbosity)"},
				  {help,      $h, "help",      undefined,        "Show this help"}
				 ]).

run(Args) ->
    case getopt:parse(?argspec, Args) of
		{ok, {Opts, [Cmd | Dirs]}} ->
			case proplists:get_bool(help, Opts) of
				true ->
					usage(),
					erlang:halt(0);
				false ->
					try start(cmd(Cmd), Dirs, Opts) of
						ok ->
							erlang:halt(0);
						{error, Err} ->
							handle_error(Err)
					catch _:Err ->
							handle_error(Err)
					end
			end;
		_ ->
			usage(),
			erlang:halt(1)
    end.


%%%
%%% Priv
%%%
-spec start(Cmd :: econfig_cmd(), Models :: [string()], Opts :: [econfig_opts()]) -> ok.
start(Cmd, Models, Opts) ->
    application:load(econfig),
    application:set_env(econfig, caller,    escript),
    application:set_env(econfig, frontend,  frontend(proplists:get_value(frontend, Opts))),
    application:set_env(econfig, log,       proplists:get_value(verbose, Opts, 0)),
    {ok, _} = application:ensure_all_started(econfig),
    Cwd = case proplists:get_value(cwd, Opts) of
			  undefined -> {ok, Dir} = file:get_cwd(), Dir;
			  Dir -> econfig_utils:canonical(filename:absname(Dir))
		  end,
    ?debug("Set current working dir: ~s", [Cwd]),
    S = econfig_state:new(Cwd),
    case econfig_state:load(S) of
		{error, _} = Err ->
			Err;
		S2 ->
			init_models(Cmd, Models, S2)
    end.

init_models(Cmd, [], State) ->
    ?debug("No model specified: load from current dir", []),
    Cwd = econfig_state:basedir(State),
    init_models(Cmd, [Cwd], State);
init_models(Cmd, ModelsDef, State) ->
    case econfig_state:parse_models(parse_models_list(ModelsDef, []), State) of
		{error, _} = Err ->
			Err;
		S2 ->
			command(Cmd, S2)
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

command(print, State) ->
    econfig_config:pp(econfig_state:config(State)),
    ok;

command(configure, State) ->
    case econfig_state:configure(State) of
		{error, Err} ->
			handle_error(Err);
		S2 ->
			econfig_state:store(S2)
    end.

cmd("configure") -> configure;
cmd("print") -> print;
cmd(Cmd) -> throw({invalid_command, Cmd}).

frontend(Frontend) -> 
    case lists:member(Frontend, [ atom_to_list(F) || F <- ?frontends]) of
		true ->
			list_to_atom(Frontend);
		false ->
			throw({invalid_frontend, Frontend})
    end.

usage() ->
    getopt:usage(?argspec, atom_to_list(?MODULE), 
				 "command [<dir ...>]",
				 [{"command", "configure | print"},
				  {"<models ...>", "Space separated list of models. "
				   "Form is 'ns:/path/to/dir' or '/path/to/dir'. In case 'ns' is ommitted, "
				   "default to basename(/path/to/dir) [default: <cwd>]"}]),
    ok.


handle_error({model_list_parse_error, Str}) ->
    io:format("E: Invalid model list definition: ~s~n", [Str]),
    erlang:halt(1);

handle_error({invalid_filename, F}) ->
    io:format("E: Invalid filename: ~p~n", [F]),
    erlang:halt(1);

handle_error({cycle, Path}) ->
    io:format("E: Cycling references in configuration: ~p~n", [Path]),
    erlang:halt(1);

handle_error({badentry, {App, Key}}) ->
    io:format("E: Invalid key: ~p:~p~n", [App, Key]),
    erlang:halt(1);

handle_error({badentry, Key}) ->
    io:format("E: Invalid key: ~p~n", [Key]),
    erlang:halt(1);

handle_error({invalid_command, V}) ->
    io:format("E: Invalid command: ~p~n", [V]),
    erlang:halt(1);

handle_error({invalid_frontend, V}) ->
    io:format("E: Invalid frontend: ~p~n", [V]),
    erlang:halt(1);

handle_error({invalid_input, V}) ->
    io:format("E: Invalid user input: ~p~n", [V]),
    erlang:halt(1);

handle_error({missing_source, F}) ->
    io:format("E: Missing template source: ~s.in~n", [F]),
    erlang:halt(1);

handle_error(Err) ->
    case econfig_log:is_debug() of
		true -> erlang:display(erlang:get_stacktrace());
		false -> ok
    end,
    io:format("E: internal error (~p)~n", [Err]),
    erlang:halt(1).
