%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_rebar).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([init/1,
	 script/2]).

init(Basedir) ->
    application:set_env(econfig, basedir, Basedir),
    application:set_env(econfig, caller, rebar),
    application:set_env(econfig, log, 100),
    application:ensure_all_started(econfig).

-spec script(Config :: [term()], Script :: file:file_all()) -> [term()].
script(Config, ScriptName) ->
    econfig_rebar:init(filename:dirname(ScriptName)),
    Basename = filename:basename(ScriptName, ".script"),
    TmplName = Basename ++ ".in",
    case filelib:is_regular(TmplName) of
	true ->
	    ?debug("Generates config from template: ~p", [TmplName]),
	    Tmpfile = econfig_utils:mktemp(Basename),
	    Data = econfig_srv:hash(),
	    Bin = bbmustache:compile(bbmustache:parse_file(TmplName), Data),
	    file:write_file(Tmpfile, Bin),
	    case file:consult(Tmpfile) of
		{ok, Config2} ->
		    file:delete(Tmpfile),
		    lists:keymerge(1, Config2, Config);
		{error, Err} ->
		    throw(Err)
	    end;
	false ->
	    Config
    end.
