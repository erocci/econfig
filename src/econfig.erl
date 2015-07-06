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
		  {help,      $h,          "help",         undefined,             "Show this help"}
		 ]).

main(Args) ->
    case getopt:parse(?argspec, Args) of
	{ok, {Opts, Files}} ->
	    case proplists:get_bool(help, Opts) of
		true ->
		    usage(),
		    erlang:halt(0);
		false ->
		    try run(Files, Opts) of
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

-spec run(Files :: [string()], Opts :: econfig_opts()) -> ok | {error, econfig_err()}.
run(Filenames, _Opts) ->
    AppEntries = lists:foldl(fun (Filename, Acc) ->
				     {App, Model} = load_model(Filename),
				     [{App, Model} | Acc]
			     end, [], Filenames),
    ConfigModel = build_model(AppEntries),
    io:format("Model: ~p~n", [ConfigModel]),
    ok.

%%%
%%% Private
%%%
load_model(Filename) ->
    case string:tokens(filename:basename(Filename), ".") of
	[App, "econfig"] ->
	    case file:consult(Filename) of
		{ok, Entries} ->
		    {list_to_atom(App), Entries};
		{error, _} = Err ->
		    throw(Err)
	    end;
	_ ->
	    throw({invalid_filename, Filename})
    end.


build_model(Entries) ->
    M0 = lists:foldl(fun ({AppName, AppEntries}, Acc) ->
			     econfig_model:add_entries(AppName, AppEntries, Acc)
		     end, econfig_model:new(), Entries),
    econfig_model:solve_deps(M0).


usage() ->
    getopt:usage(?argspec, atom_to_list(?MODULE), 
		 "<appname.econfig ...>",
		 [{"<appname.econfig ...>", "List of appname.econfig files to consult"}]),
    ok.


handle_error({invalid_filename, F}) ->
    ?error("Invalid filename: ~p", [F]),
    erlang:halt(1);

handle_error({cycle, Path}) ->
    ?error("Cycling references in configuration: ~p", [Path]),
    erlang:halt(1);

handle_error({badentry, {App, Key}}) ->
    ?error("Invalid key: ~p:~p", [App, Key]),
    erlang:halt(1);

handle_error(Err) ->
    ?error(Err),
    erlang:halt(1).
