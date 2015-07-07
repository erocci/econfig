%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_frontend).

-include("econfig.hrl").

-export([new/1,
	 run/2]).

-type ui() :: term().

-record state, {
	  mod      :: atom(),
	  ref      :: term(),
	  model    :: econfig_model:t()
	 }.
-type t() :: #state{}.
-export_type([t/0, ui/0]).

-callback start_link(Model :: econfig_model:t(), Opts :: term()) -> {ok, ui()} | {error, econfig_err()}.
-callback ask(Entry :: econfig_entry(), Config :: econfig_config:t(), ui()) -> {ok, econfig_value(), ui()}.
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

-spec run(Config :: econfig_config:t(), Frontend :: t()) -> {ok, econfig_config:t()} | {error, econfig_err()}.
run(Config, #state{model=Model}=F) ->
    {C1, _} = lists:foldl(fun (Entry, {C0, F0}) ->
				  eval(Entry, C0, F0)
			  end, {Config, F}, econfig_model:entries(Model)),
    {ok, C1}.

%%%
%%% Priv
%%%
eval({Key, _, _, _, _} = Entry, Config, #state{mod=Mod, ref=Ref}=F) ->
    case econfig_config:lookup(Key, Config) of
	{ok, _V} ->
	    % If already set, pass
	    {Config, F};
	undefined ->
	    {ok, Val, Ref} = Mod:ask(Entry, Config, Ref),
	    econfig_config:set(Key, Val, Config),
	    {Config, F#state{ref=Ref}}
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
