%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_config).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([new/1,
	 lookup/2,
	 set/3,
	 export/1,
	 export/2]).

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

-spec export(t()) -> ok | {error, econfig_err()}.
export(C) ->
    case file:get_cwd() of
	{ok, Dir} -> export(C, Dir);
	{error, _} = Err -> Err
    end.
	    

-spec export(t(), Dir :: file:filename()) -> ok | {error, econfig_err()}.
export(#state{tid=Tid}, Dir) ->
    Filename = filename:join([Dir, ?econfig_file]),
    case file:open(Filename, [write]) of
	{ok, File} ->
	    C = ets:foldl(fun ({{App, Key}, Val}, Acc) ->
				  [{App, Key, Val} | Acc]
			  end, [], Tid),
	    ok = io:fwrite(File, "~p~n", [C]),
	    file:close(File),
	    ?info("Config written in ~s~n", [Filename]),
	    ok;
	{error, _} = Err ->
	    Err
    end.


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
