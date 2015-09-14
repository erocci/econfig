%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_frontend).

-include("econfig.hrl").
-include("econfig_log.hrl").

% External API
-export([new/1,
	 run/2]).

% Frontend implementations API
-export([is_enable/3,
	 eval/4]).

-type ui() :: term().

-record state, {
	  mod      :: atom(),
	  ref      :: term(),
	  model    :: econfig_model:t()
	 }.
-type t() :: #state{}.
-export_type([t/0, ui/0]).

-callback start_link(Model :: econfig_model:t(), Opts :: term()) -> {ok, ui()} | {error, econfig_err()}.
-callback run(Model :: econfig_model:t(), Config :: econfig_config:t(), ui()) -> {ok, econfig_config:t(), ui()} 
										     | {error, term()}.
-callback terminate(Ref :: ui()) -> ok.

-spec new(Model :: econfig_model:t()) -> {ok, t()} | {error, econfig_err()}.
new(Model) ->
    Mod = impl(),
    case Mod:start_link(Model, []) of
	{ok, Ref} ->
	    #state{mod=Mod, ref=Ref, model=Model};
	{error, _} = Err ->
	    Err
    end.

-spec run(Config :: econfig_config:t(), Frontend :: t()) -> {ok, econfig_config:t(), t()} | {error, econfig_err()}.
run(Config, #state{model=Model, mod=Mod, ref=Ref}=F) ->
    case Mod:run(Model, Config, Ref) of
	{ok, C1, Ref1} ->
	    {ok, C1, F#state{ref=Ref1}};
	{error, _} = Err ->
	    Err
    end.

-spec is_enable(econfig_entry:t(), econfig_config:t(), econfig_model:t()) -> boolean.
is_enable(Entry, Config, Model) ->
    is_enable_(econfig_model:deps(Entry, Model), Config).


-spec eval(Entry :: econfig_entry:t(), Fun :: fun(), Config :: econfig_config:t(), Model :: econfig_model:t()) -> econfig_config:t().
eval(Entry, Fun, Config, Model) ->
    Val = case is_enable(Entry, Config, Model) of
	      true ->
		  econfig_entry:eval(Fun, Entry);
	      false ->
		  disable(econfig_entry:type(Entry))
	  end,
    econfig_config:set(econfig_entry:key(Entry), Val, Config).

%%%
%%% Priv
%%%
is_enable_([], _) -> 
    true;
is_enable_([ Dep | Tail ], Config) ->
    case econfig_config:lookup(econfig_dep:key(Dep), Config) of
	{ok, Val} -> 
	    case econfig_dep:match(Dep, Val) of
		true -> is_enable_(Tail, Config);
		false -> false
	    end;
	undefined -> false
    end.

impl() ->
    case application:get_env(econfig, frontend, tty) of
	I when is_atom(I) ->
	    Mod = list_to_atom("econfig_ui_" ++ atom_to_list(I)),
	    case is_module(Mod) of
		true ->
		    Mod;
		false ->
		    throw({invalid_frontend, Mod})
	    end;
	Else ->
	    throw({invalid_frontend, Else})
    end.

is_module(Mod) when is_atom(Mod) ->
    try Mod:module_info() of
        _ -> true
    catch _:_ -> false
    end;
is_module(_) -> 
    false.


disable(boolean) -> false;
disable(_) -> undefined.
