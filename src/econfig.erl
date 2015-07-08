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
-export([start/3,
	 run/2,
	 command/2]).

-include("econfig.hrl").
-include("econfig_log.hrl").

-type econfig_cmd() :: configure | print.
-type econfig_opts() :: {frontend, tty}.

-define(frontends_str, 
	io_lib:format("(~s)", [string:join([ atom_to_list(F) || F <- ?frontends], "|")])).

-define(argspec, [
		  {frontend, $f, "frontend", {string, "tty"}, "User frontend " ++ ?frontends_str},
		  {verbose,  $v, "verbose",  integer,            "Verbose (multiple times increase verbosity)"},
		  {help,     $h, "help",     undefined,          "Show this help"}
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

-spec start(Cmd :: econfig_cmd(), Filenames :: [string()], Opts :: econfig_opts()) -> ok.
start(Cmd, Files, Opts) ->
    application:load(econfig),
    application:set_env(econfig, frontend, frontend(proplists:get_value(frontend, Opts))),
    {ok, _} = application:ensure_all_started(econfig),
    run(Cmd, Files).

-spec run(Cmd :: econfig_cmd(), Files :: [string()]) -> ok | {error, econfig_err()}.
run(Cmd, Filenames) ->
    case load_models(Filenames) of
	{ok, ConfigModel} ->
	    command(Cmd, ConfigModel);
	{error, _} = Err ->
	    Err
    end.

command(print, Model) ->
    econfig_model:pp(Model),
    ok;

command(configure, Model) ->
    C = econfig_config:new(Model),
    F = econfig_frontend:new(Model),
    case econfig_frontend:run(C, F) of
	{ok, C1} ->
	    io:format("~p~n", [econfig_config:export(C1)]),
	    ok;
	{error, _} = Err ->
	    Err
    end;

command(undefined, Model) ->
    Model.


%%%
%%% Private
%%%
load_models(Filenames) ->
    AppEntries = lists:foldl(fun (Filename, Acc) ->
				     case load_model(Filename) of
					 {ok, {App, Model}} ->
					     [{App, Model} | Acc];
					 {error, bad_name} ->
					     % Simply ignore bad files
					     ?info("W: Ignoring invalid file ~s~n", [Filename]),
					     Acc;
					 {error, _} = Err ->
					     throw(Err)
				     end
			     end, [], Filenames),
    ConfigModel = build_model(AppEntries),
    {ok, ConfigModel}.

load_model(Filename) ->
    case string:tokens(filename:basename(Filename), ".") of
	[App, "econfig"] ->
	    case file:consult(Filename) of
		{ok, Entries} ->
		    {ok, {list_to_atom(App), Entries}};
		{error, _} = Err ->
		    Err
	    end;
	_ ->
	    {error, bad_name}
    end.


build_model(Entries) ->
    M0 = econfig_model:new(),
    Model = lists:foldl(fun ({AppName, AppEntries}, Acc) ->
				econfig_model:add_entries(AppName, AppEntries, Acc)
			end, M0, Entries),
    econfig_model:solve_deps(Model).


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
