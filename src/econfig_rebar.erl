%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_rebar).

-include("econfig.hrl").
-include("econfig_log.hrl").

-define(KEY, 'econfig.state').

-export([init/1,
	 state/1,
	 state/2,
	 fold_apps/3,
	 script/4]).

-spec init(rebar_state:t()) -> econfig_state:t().
init(Rebar) ->
    Basedir = rebar_dir:base_dir(Rebar),
    Ecfg = init_(Basedir),
    rebar_state:set(Rebar, ?KEY, Ecfg).

-spec state(rebar_state:t()) -> econfig_state:t().
state(Rebar) ->
    rebar_state:get(Rebar, ?KEY).

-spec state(econfig_state:t(), rebar_state:t()) -> rebar_state:t().
state(Ecfg, Rebar) ->
    rebar_state:set(Rebar, ?KEY, Ecfg).

-spec fold_apps(Fun :: atom(), Acc :: any(), rebar_state:t()) -> any().
fold_apps(Fun, Acc, State) ->
    ProjectApps = rebar_state:project_apps(State),
    Deps = rebar_state:all_deps(State),
    fold_app(Fun, ProjectApps ++ Deps, Acc, State).


-spec script(AppName :: atom(), Basedir :: filename:file(), Config :: [term()], Target :: filename:file()) -> [term()].
script(AppName, Basedir, Config, Target) ->
    Ecfg = init_(Basedir),
    Tmpl = Target ++ ".in",
    Tmpfile = econfig_utils:mktemp(Target),
    ok = econfig_utils:gen(AppName, Ecfg, Tmpfile, Tmpl),
    case file:consult(Tmpfile) of
	{ok, Config2} ->
	    file:delete(Tmpfile),
	    Config3 = lists:keymerge(1, Config2, Config),
	    Config3;
	{error, _} ->
	    Config
    end.

%%%
%%% Priv
%%%
init_(Basedir) ->
    application:set_env(econfig, caller, rebar),
    application:set_env(econfig, log, 100),
    application:ensure_all_started(econfig),
    econfig_state:new(Basedir).

fold_app(_, [], Acc, _State) ->
    Acc;
fold_app(Fun, [ App | Tail ], Acc, State) ->
    AppState = rebar_app_info:state_or_new(State, App),
    fold_app(Fun, Tail, Fun(AppState, App, Acc), State).


