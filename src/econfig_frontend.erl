%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_frontend).

-include("econfig.hrl").
-include("econfig_log.hrl").

%% External API
-export([new/2,
		 run/2]).

-type ui() :: term().

-record(state, {mod      :: atom(),
				ref      :: term(),
				model    :: econfig_model:t()}).
-type t() :: #state{}.
-export_type([t/0, ui/0]).

-callback start_link(Model :: econfig_model:t(), Opts :: term()) -> {ok, ui()} | {error, econfig_err()}.
-callback run(Model :: econfig_model:t(), Config :: econfig_config:t(), ui()) -> {ok, econfig_config:t(), ui()} 
																					 | {error, term()}.
-callback terminate(Ref :: ui()) -> ok.
-callback format_error(Err :: term()) -> ok.

-spec new(Frontend :: atom(), Model :: econfig_model:t()) -> {ok, t()} | {error, econfig_err()}.
new(Frontend, Model) ->
    Mod = impl(Frontend),
    case Mod:start_link(Model, []) of
		{ok, Ref} ->
			#state{mod=Mod, ref=Ref, model=Model};
		{error, _} = Err ->
			Err
    end.

-spec run(Config :: econfig_config:t(), Frontend :: t()) -> {ok, econfig_config:t(), t()} | {error, econfig_err()}.
run(Config, #state{model=Model, mod=Mod, ref=Ref}=F) ->
    try Mod:run(Model, Config, Ref) of
		{ok, C1, Ref1} ->
			{ok, C1, F#state{ref=Ref1}};
		{error, _} = Err ->
			Mod:format_error(Err),
			Err
    catch
		throw:Err ->
			Mod:format_error(Err),
			Err
    end.

%%%
%%% Priv
%%%
impl(Frontend) ->
	Mod = list_to_atom("econfig_ui_" ++ atom_to_list(Frontend)),
	case is_module(Mod) of
		true ->
			Mod;
		false ->
			throw({invalid_frontend, Mod})
	end.

is_module(Mod) when is_atom(Mod) ->
    try Mod:module_info() of
        _ -> true
    catch _:_ -> false
    end;
is_module(_) -> 
    false.
