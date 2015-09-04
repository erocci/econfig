%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(econfig_prv_configure).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("econfig_log.hrl").

-define(PROVIDER, configure).
-define(DEPS, [{default, lock}]).

-define(PRE_HOOKS, []).
-define(POST_HOOKS, []).
-define(HOOKS, {?PRE_HOOKS, ?POST_HOOKS}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    P = providers:create([{name, ?PROVIDER},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, " econfig configure <options>"},
                          {short_desc, "Configure application build."},
                          {desc, "Configure application build."},
                          {hooks, ?HOOKS},
                          {opts, []},
                          {namespace, econfig}]),
    State1 = rebar_state:add_provider(State, P),    
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?info("Configuring application...", []),
    econfig:models(foreach_apps(fun (AppState, AppInfo) ->
                                        app_config_model(AppState, AppInfo)
                                end, State)),
    case econfig:configure() of
        {ok, C} ->
            econfig_config:store(C),
            {ok, State};
        {error, _} = Err ->
            Err
    end.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% Private
%%
foreach_apps(Fun, State) ->
    ProjectApps = rebar_state:project_apps(State),
    Deps = rebar_state:all_deps(State),
    [ Fun(rebar_app_info:state_or_new(State, App), App) || App <- ProjectApps ++ Deps ].


app_config_model(State, Info) ->
    AppName = rebar_app_info:name(Info),
    ?debug("Load configuration entries for ~s", [AppName]),
    { AppName, rebar_state:get(State, econfig, [])}.
