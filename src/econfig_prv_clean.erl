%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(econfig_prv_clean).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("econfig_log.hrl").

-define(PROVIDER, clean).
-define(DEPS, [lock]).

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
                          {example, " econfig clean <options>"},
                          {short_desc, "Cleanup configuration."},
                          {desc, "Cleanup configuration."},
                          {hooks, ?HOOKS},
                          {opts, []},
                          {namespace, econfig}]),
    State1 = rebar_state:add_provider(State, P),    
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?info("Clean configuration...", []),
    econfig_config:clean(),
    {ok, State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% Private
%%
