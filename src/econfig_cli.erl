%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cli).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([run/1,
		 handle_error/1]).

%% Exported for econfig_cmd_help
-export([cmd_names/0,
		 cmd_mod/1]).

-type econfig_opts() :: {frontend, tty}.

-define(commands, [econfig_cmd_configure, 
				   econfig_cmd_print, 
				   econfig_cmd_help]).

-define(argspec, [
				  {cwd,       $C, "dir",       string,           "Change working directory"},
				  {verbose,   $v, "verbose",   integer,          "Verbose (multiple times increase verbosity)"},
				  {help,      $h, "help",      undefined,        "Show this help"}
				 ]).

run(Args) ->
    case getopt:parse(?argspec, Args) of
		{ok, {Opts, [Cmd | CmdOpts]}} ->
			Mod = cmd_mod(Cmd),
			case proplists:get_bool(help, Opts) of
				true ->
					usage(),
					erlang:halt(0);
				false ->
					try start(Mod, Opts, CmdOpts) of
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
-spec start(Cmd :: atom(), Opts :: [econfig_opts()], CmdOpts :: [term()]) -> ok.
start(econfig_cmd_help, _Opts, CmdArgs) ->
	econfig_cmd_help:run(undefined, CmdArgs);
start(Mod, Opts, CmdArgs) ->
    application:load(econfig),
    application:set_env(econfig, caller,    escript),
    application:set_env(econfig, log,       proplists:get_value(verbose, Opts, 0)),
    {ok, _} = application:ensure_all_started(econfig),
    Cwd = case proplists:get_value(cwd, Opts) of
			  undefined -> {ok, Dir} = file:get_cwd(), Dir;
			  Dir -> econfig_utils:canonical(filename:absname(Dir))
		  end,
    ?debug("Set current working dir: ~s", [Cwd]),
    State = econfig_state:new(Cwd),
	Mod:run(State, CmdArgs).

cmd_mod(Name) -> cmd_mod(Name, ?commands).

cmd_mod(Name, []) -> 
	throw({invalid_command, Name});
cmd_mod(Name, [Mod | Tail]) ->
	case atom_to_list(cmd_attr(cmd_name, Mod)) of
		Name -> Mod;			
		_ -> cmd_mod(Name, Tail)
	end.

usage() ->
    getopt:usage(?argspec, "econfig", 
				 "command <command_opts>", []),
	io:format("  ~-17s" ++ string:join(cmd_names(), " | ") ++ "~n", ["Commands:"]),
	lists:foreach(fun (Mod) ->
						  io:format("  * ~-15s", [atom_to_list(cmd_attr(cmd_name, Mod))]),
						  io:format("~s~n", [cmd_attr(cmd_desc, Mod)])
				  end, ?commands),
    ok.

cmd_names() ->
	lists:map(fun (Mod) -> atom_to_list(cmd_attr(cmd_name, Mod)) end, ?commands).

cmd_attr(Key, Mod) ->
	case proplists:get_value(Key, Mod:module_info(attributes)) of
		[Attr|_] when is_atom(Attr) -> Attr;
		Attr when is_list(Attr) -> Attr;
		_ -> throw(io_lib:format("Invalid command attribute: ~s", [Key]))
	end.

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
	handle_error_gen(Err).

handle_error_gen(Err) when is_list(Err) ->
    case econfig_log:is_debug() of
		true -> erlang:display(erlang:get_stacktrace());
		false -> ok
    end,
    io:format("E: internal error (~s)~n", [Err]),
    erlang:halt(1);
handle_error_gen(Err) ->
	handle_error_gen(io_lib:format("~p", [Err])).
