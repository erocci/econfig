%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_config).

-include("econfig.hrl").

-export([new/1,
	 lookup/2,
	 set/3,
	 export/1]).

-record state, {
	  tid        :: ets:tid(),
	  model      :: econfig_model:t()
	 }.
-type t() :: #state{}.
-export_type([t/0]).

-spec new(Model :: econfig_model:t()) -> t().
new(Model) ->
    Tid = ets:new(config, []),
    #state{model=Model, tid=Tid}.

-spec export(t()) -> econfig_config().
export(#state{tid=Tid}) ->
    ets:foldl(fun ({{App, Key}, Val}, Acc) ->
		      [{App, Key, Val} | Acc]
	      end, [], Tid).


-spec lookup(Key :: econfig_entry_key(), t()) -> {ok, econfig_value()} | undefined.
lookup(Key, #state{tid=Tid}) ->
    case ets:lookup(Tid, Key) of
	[] -> undefined;
	[{_Key, Val} | _] -> Val     % If multiple values, undefined
    end.


-spec set(Key :: econfig_entry_key(), Val :: econfig_value(), t()) -> ok.
set(Key, Val, #state{tid=Tid}) ->
    ets:insert(Tid, {Key, Val}).

%%%
%%% Priv
%%%
