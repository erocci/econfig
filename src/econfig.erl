%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig).

% Script mode
-export([main/1]).

% API
-export([load/1,
	 configure/0]).

-include("econfig.hrl").
-include("econfig_log.hrl").

-type econfig_cmd() :: configure | print.
-type econfig_opts() :: {frontend, tty}.

-define(frontends_str, 
	io_lib:format("(~s)", [string:join([ atom_to_list(F) || F <- ?frontends], "|")])).

-define(argspec, [
		  {frontend, $f, "frontend", {string, "tty"}, "User frontend " ++ ?frontends_str},
		  {verbose,  $v, "verbose",  integer,         "Verbose (multiple times increase verbosity)"},
		  {help,     $h, "help",     undefined,       "Show this help"}
		 ]).

main(Args) ->
    case getopt:parse(?argspec, Args) of
	{ok, {Opts, [Cmd | Files]}} ->
	    case proplists:get_bool(help, Opts) of
		true ->
		    usage(),
		    erlang:halt(0);
		false ->
		    try start(cmd(Cmd), Files, Opts) of
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

load(Dirs) ->
    econfig_srv:load(Dirs).

configure() ->
    econfig_srv:configure().

%%%
%%% Private
%%%
-spec start(Cmd :: econfig_cmd(), Filenames :: [string()], Opts :: econfig_opts()) -> ok.
start(Cmd, Files, Opts) ->
    application:load(econfig),
    application:set_env(econfig, frontend, frontend(proplists:get_value(frontend, Opts))),
    {ok, _} = application:ensure_all_started(econfig),
    case econfig_srv:models(Files) of
	ok ->
	    command(Cmd);
	{error, _} = Err ->
	    Err
    end.

command(print) ->
    econfig_srv:print(),
    ok;

command(configure) ->
    configure().

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
		 "command <appname.econfig ...>",
		 [{"command", "configure | print"},
		  {"<appname.econfig ...>", "List of appname.econfig files to consult (ignore invalid extensions)"}]),
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

handle_error({invalid_command, V}) ->
    io:format("E: Invalid command: ~p~n", [V]),
    erlang:halt(1);

handle_error({invalid_frontend, V}) ->
    io:format("E: Invalid frontend: ~p~n", [V]),
    erlang:halt(1);

handle_error({invalid_input, V}) ->
    io:format("E: Invalid user input: ~p~n", [V]),
    erlang:halt(1);

handle_error(Err) ->
    io:format("E: ~p~n", [Err]),
    erlang:halt(1).
