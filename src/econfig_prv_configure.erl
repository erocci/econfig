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
    Ecfg = econfig_rebar:state(State),
    Models = econfig_rebar:fold_apps(fun (AppState, AppInfo, Acc) ->
                                             [ app_config_model(AppState, AppInfo) | Acc ]
                                     end, [], State),
    case econfig_state:models(Models, Ecfg) of
        {error, _} = Err -> Err;
        Ecfg2 ->
            case econfig_state:load(Ecfg2) of
                {error, enoent} -> 
                    configure(Ecfg2, State);
                {error, _} = Err -> Err;
                Ecfg3 ->
                    configure(Ecfg3, State)
            end
    end.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% Private
%%
app_config_model(_State, Info) ->
    % Reload rebar.config to get proper model, not parent's one
    Dir = rebar_app_info:dir(Info),
    Config = rebar_config:consult(Dir),
    AppName = rebar_app_info:name(Info),
    ?debug("Load configuration entries for ~s", [AppName]),
    { AppName, proplists:get_value(model, proplists:get_value(econfig, Config, []), []) }.

configure(Ecfg, Rebar) ->
    case econfig_state:configure(Ecfg) of
        {error, _} = Err ->
            Err;
        Ecfg2 ->
            ok = econfig_state:store(Ecfg2),
            {ok, econfig_rebar:state(Ecfg2, Rebar)}
    end.
