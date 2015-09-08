%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_config).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([new/0,
	 new/1,
	 load/2,
	 lookup/2,
	 set/3,
	 store/2,
	 render/3,
	 render/4,
	 hash/1,
	 hash/2]).

-record state, {
	  tid        :: ets:tid()
	 }.
-type t() :: #state{}.
-export_type([t/0]).

-spec new() -> t().
new() ->
    #state{tid=ets:new(config, [])}.

-spec new(Econfig :: filename:file()) -> t().
new(Econfig) ->
    S = new(),
    case application:get_env(econfig, overwrite, false) of
	true ->
	    S;
	false ->
	    case load(Econfig, S) of
		{error, enoent} -> S;
		{error, Err} -> throw(Err);
		#state{} = S1 -> S1
	    end
    end.

-spec load(filename:file(), t()) -> t() | {error, term()}.
load(Filename, S) ->
    case file:consult(Filename) of
	{ok, Config} ->
	    ?debug("Load config from ~s", [Filename]),
	    populate(Config, S);
	{error, _} = Err ->
	    ?debug("No config found in ~s", [Filename]),
	    Err
    end.

-spec store(filename:file(), t()) -> ok | {error, econfig_err()}.
store(Filename, #state{}=S) ->
    ?debug("Writing config to ~s", [Filename]),
    case file:write_file(Filename, [ io_lib:format("~tp.~n", [Term]) || Term <- to_list(S) ]) of
	ok ->
	    ?info("Config written in ~s", [Filename]),
	    ok;
	{error, _} = Err ->
	    Err
    end.

-spec render(LocalNS :: atom(), Filename :: file:name_all(), Config :: t()) -> ok | {error, econfig_err()}.
render(LocalNS, Target, Config) ->
    render(LocalNS, Target, #{}, Config).


-spec render(LocalNS :: atom(), Filename :: file:name_all(), Data :: #{}, Config :: t()) -> ok | {error, econfig_err()}.
render(LocalNS, Target, Data, #state{}=Config) ->
    TmplName = Target ++ ".in",
    case filelib:is_regular(TmplName) of
	true ->
	    Tmpl = bbmustache:parse_file(TmplName),
	    Data2 = hash(LocalNS, Data, Config),
	    file:write_file(Target, bbmustache:compile(Tmpl, Data2));
	false ->
	    throw({missing_source, TmplName})
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

-spec hash(Config :: t()) -> #{}.
hash(Config) ->
    hash(#{}, Config).

-spec hash(Data :: #{}, Config :: t()) -> #{}.
hash(Data, Config) ->
    hash(undefined, Data, Config).

%%%
%%% Priv
%%%
hash(LocalNS, Data, #state{tid=Tid}) ->
    ets:foldl(fun ({{App, Key}, Val}, Acc) ->
		      Acc2 = case App of
				 LocalNS -> Acc#{ atom_to_list(Key) => Val };
				 _ -> Acc
			     end,
		      Acc2#{ render_key(App, Key) => Val }
	      end, Data, Tid).


-spec populate(econfig_config(), t()) -> t().
populate(C, S) ->
    lists:foreach(fun ({Key, Val}) ->
			  set(parse_key(Key), Val, S)
		  end, C),
    S.


to_list(#state{tid=Tid}) ->
    ets:foldl(fun ({{App, Key}, Val}, Acc) ->
		      [{render_key(App, Key), Val} | Acc]
	      end, [], Tid).


parse_key(Key) ->
    case string:tokens(Key, ".") of
	[App | Rest] ->
	    {list_to_atom(App), list_to_atom(string:join(Rest, "."))};
	_ ->
	    throw({badentry, Key})
    end.


render_key(App, Key) ->
    string:join([atom_to_list(App), atom_to_list(Key)], ".").
