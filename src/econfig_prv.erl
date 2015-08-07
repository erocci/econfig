%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(econfig_prv).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("econfig_log.hrl").

-define(PROVIDER, configure).
-define(DEPS, [lock]).

-define(PRE_HOOKS, [{compile, configure}]).
-define(POST_HOOKS, []).
-define(HOOKS, {?PRE_HOOKS, []}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    P = providers:create([{name, ?PROVIDER},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, " configure <options>"},
                          {short_desc, "Configure application build."},
                          {desc, "Configure application build."},
                          {hooks, ?HOOKS},
                          {opts, []}]),
    State1 = rebar_state:add_provider(State, P),    
    application:set_env(econfig, basedir, rebar_dir:root_dir(State)),
    application:set_env(econfig, caller, rebar),
    application:ensure_all_started(econfig),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?info("Configuring the build...", []),
    econfig:models(apps_config_model(State)),
    econfig:configure(),
    {ok, State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% Private
%%
apps_config_model(State) ->
    ProjectApps = rebar_state:project_apps(State),
    Deps = rebar_state:all_deps(State),
    [ app_config_model(State, App) || App <- ProjectApps ++ Deps ].


app_config_model(State, AppInfo) ->
    AppName = rebar_app_info:name(AppInfo),
    ?debug("Load configuration entries for app: ~s", [AppName]),
    S = rebar_app_info:state_or_new(State, AppInfo),
    { AppName, rebar_state:get(S, econfig, [])}.
