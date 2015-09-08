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
-spec start(Cmd :: econfig_cmd(), Dirs :: [filename:file()], Opts :: [econfig_opts()]) -> ok.
start(Cmd, Dirs, Opts) ->
    application:load(econfig),
    application:set_env(econfig, frontend,  frontend(proplists:get_value(frontend, Opts))),
    application:set_env(econfig, log,       proplists:get_value(verbose, Opts, 0)),
    {ok, _} = application:ensure_all_started(econfig),
    case econfig_srv:load(Dirs) of
	ok ->
	    command(cmd(Cmd));
	{error, _} = Err ->
	    Err
    end.

command(print) ->
    econfig_srv:print(),
    ok;

command(configure) ->
    case econfig_srv:configure() of
	{ok, C} ->
	    econfig_config:store(C);
	{error, Err} ->
	    handle_error(Err)
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
		 "command <dir ...>",
		 [{"command", "configure | print"},
		  {"<dir ...>", "List of dirs containing Econfig files"}]),
    ok.


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
    io:format("E: ~p~n", [Err]),
    erlang:halt(1).
