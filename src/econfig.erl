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
-export([run/2]).

-include("econfig.hrl").
-include("econfig_log.hrl").

-type econfig_opts() :: [].

-define(argspec, [
		  {verbose, $v, "verbose", integer,   "Verbose (multiple times increase verbosity)"},
		  {help,    $h, "help",    undefined, "Show this help"}
		 ]).

main(Args) ->
    case getopt:parse(?argspec, Args) of
	{ok, {Opts, Files}} ->
	    case proplists:get_bool(help, Opts) of
		true ->
		    usage(),
		    erlang:halt(0);
		false ->
		    start(Files, Opts)
	    end;
	_ ->
	    usage(),
	    erlang:halt(1)
    end.

-spec run(Files :: [string()], Opts :: econfig_opts()) -> ok | {error, econfig_err()}.
run(Filenames, _Opts) ->
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
    econfig_model:pp(ConfigModel),
    ok.

%%%
%%% Private
%%%
start(Files, Opts) ->
    application:load(econfig),
    {ok, _} = application:ensure_all_started(econfig),
    ?debug("econfig ~p~n", [Files]),
    try run(Files, Opts) of
	ok ->
	    erlang:halt(0);
	{error, Err} ->
	    handle_error(Err)
    catch _:Err ->
	    handle_error(Err)
    end.

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


usage() ->
    getopt:usage(?argspec, atom_to_list(?MODULE), 
		 "<appname.econfig ...>",
		 [{"<appname.econfig ...>", "List of appname.econfig files to consult"}]),
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

handle_error(Err) ->
    io:format("E: ~p~n", [Err]),
    erlang:halt(1).
