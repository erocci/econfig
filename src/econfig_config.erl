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
	 load/1,
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
    case application:get_env(econfig, overwrite, false) of
	true ->
	    #state{model=Model, tid=Tid};
	false ->
	    load(#state{model=Model, tid=Tid})
    end.

-spec load(t()) -> t().
load(S) ->
    Filename = get_econfig_name(),
    case file:consult(Filename) of
	{ok, [Config]} ->
	    ?info("Load config from ~s~n", [Filename]),
	    populate(Config, S);
	{ok, _} ->
	    {error, {invalid_config, Filename}};
	{error, enoent} ->
	    S;
	{error, Err} ->
	    ?warn("Can not read configuration: ~p~n", [Err]),
	    S
    end.

-spec export(t()) -> ok | {error, econfig_err()}.
export(#state{tid=Tid}) ->
    Filename = get_econfig_name(),
    case file:open(Filename, [write]) of
	{ok, File} ->
	    C = ets:foldl(fun ({{App, Key}, Val}, Acc) ->
				  [{App, Key, Val} | Acc]
			  end, [], Tid),
	    ok = io:fwrite(File, "~p.~n", [C]),
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
	[{_Key, Val} | _] -> {ok, Val}
    end.


-spec set(Key :: econfig_entry_key(), Val :: econfig_value(), t()) -> ok.
set(Key, Val, #state{tid=Tid}) ->
    ets:insert(Tid, {Key, Val}).

%%%
%%% Priv
%%%
-spec populate(econfig_config(), t()) -> t().
populate(C, S) ->
    lists:foreach(fun ({App, Key, Val}) ->
			  set({App, Key}, Val, S)
		  end, C),
    S.

get_econfig_name() ->
    filename:join([application:get_env(econfig, basedir, ""), ?econfig_file]).
