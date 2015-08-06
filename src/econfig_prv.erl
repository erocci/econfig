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
    ProjectApps = rebar_state:project_apps(State),
    [ configure(State, App) || App <- ProjectApps ],
    {ok, State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% Private
%%
all_app_dirs(State) ->
    Deps = rebar_state:lock(State),
    Apps = rebar_state:project_apps(State),
    [ rebar_app_info:dir(App) || App <- Apps ++ Deps ].

configure(State, AppInfo) ->
    ?info("Configuring ~s", [rebar_app_info:name(AppInfo)]),
    econfig:load(all_app_dirs(State)),
    ?info("Create files: ~p", [rebar_state:get(State, econfig_files, [])]),
    {ok, State}.
