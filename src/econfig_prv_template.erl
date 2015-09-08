%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(econfig_prv_template).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("econfig_log.hrl").

-define(PROVIDER, template).
-define(DEPS, [{econfig, configure}]).

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
                          {example, " econfig template <options>"},
                          {short_desc, "Instantiate config template if any."},
                          {desc, "Instantiate config template if any."},
                          {hooks, ?HOOKS},
                          {opts, []},
                          {namespace, econfig}]),
    State1 = rebar_state:add_provider(State, P),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    S1 = econfig_rebar:fold_apps(
           fun (AppState, AppInfo, StateAcc) ->
                   do_app_templates(AppState, AppInfo, StateAcc)
           end, State, State),
    {ok, S1}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% Private
%%
do_app_templates(AppState, _AppInfo, State) ->
    Files = proplists:get_value(files, rebar_state:get(AppState, econfig, []), []),
    {DynamicFiles, OtherFiles} = filter_files(Files),
    lists:foreach(fun (F) -> do_dynamic(F, State) end, DynamicFiles),
    lists:foreach(fun (F) -> do_static(F, State) end, OtherFiles),
    State.

do_dynamic(Filename, State) ->
    Tmpl = filename:join([code:priv_dir(econfig), "script.tmpl"]),
    Data = #{ "econfig.paths" => code_path(),
              "econfig.basedir" => rebar_dir:base_dir(State),
              "econfig.target" => Filename },
    Bin = bbmustache:compile(bbmustache:parse_file(Tmpl), Data),
    file:write_file(Filename ++ ".script", Bin).

do_static(Filename, State) ->
    econfig_utils:gen(rebar_dir:basedir(State) ++ ".econfig", Filename, Filename ++ ".in").

filter_files(Files) ->
    lists:foldl(
      fun (Filename, {DynAcc, OtherAcc}) ->
              case filename:basename(Filename) of
                  "rebar.config" ->
                      {[Filename | DynAcc], OtherAcc};
                  _ ->
                      case lists:reverse(string:tokens(Filename, ".")) of
                          ["src", "app" | _] -> {[Filename | DynAcc], OtherAcc};
                          _ -> {DynAcc, [Filename | OtherAcc]}
                      end
              end
      end, {[], []}, Files).

code_path() ->
    io_lib:format("[~s]", [string:join([ io_lib:format("\"~s\"", [P]) || P <- code:get_path() ], ", \n")]).
