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
		 load/2,
		 lookup/2,
		 set/3,
		 store/2,
		 compile/2,
		 render/2,
		 render/4,
		 hash/1,
		 hash/2,
		 hash/3,
		 to_list/1]).

-record state, {
		  tid        :: ets:tid()
		 }.
-type t() :: #state{}.
-export_type([t/0]).

-spec new() -> t().
new() ->
    #state{tid=ets:new(config, [])}.

-spec load(filename:file(), t()) -> t() | {error, term()}.
load(Filename, S) ->
    case file:consult(Filename) of
		{ok, Config} ->
			?debug("Load config from ~s", [Filename]),
			populate(Config, S);
		{error, enoent} ->
			?debug("No config found in ~s", [Filename]),
			S;
		{error, _} = Err ->
			?debug("Error loading ~s", [Filename]),
			Err
    end.


-spec store(filename:file(), t()) -> ok | {error, econfig_err()}.
store(Filename, #state{}=S) ->
    ?debug("Writing config to ~s", [Filename]),
    case file:write_file(Filename, [ io_lib:format("~tp.~n", [Term]) || Term <- to_list(S) ]) of
		ok ->
			?debug("Config written in ~s", [Filename]),
			ok;
		{error, _} = Err ->
			Err
    end.

-spec compile(In :: iodata(), Config :: t()) -> iodata() | {error, econfig_err()}.
compile(In, Config) ->
	Tmpl = bbmustache:parse_binary(In),
	Data2 = hash(undefined, #{}, Config),
	bbmustache:compile(Tmpl, Data2).


-spec render(Filename :: file:name_all(), Config :: t()) -> 
					iodata() | {error, econfig_err()}.
render(Filename, Config) ->
	render(undefined, Filename, #{}, Config).

-spec render(LocalNS :: atom(), Filename :: file:name_all(), Data :: #{}, Config :: t()) -> 
					iodata() | {error, econfig_err()}.
render(LocalNS, Filename, Data, #state{}=Config) ->
    case filelib:is_regular(Filename) of
		true ->
			Tmpl = bbmustache:parse_file(Filename),
			Data2 = hash(LocalNS, Data, Config),
			bbmustache:compile(Tmpl, Data2);
		false ->
			throw({invalid_filename, Filename})
    end.


-spec lookup(Key :: econfig_entry_key(), t()) -> {ok, econfig_value()} | undefined.
lookup(Key, #state{tid=Tid}) ->
    case ets:lookup(Tid, Key) of
		[] -> undefined;
		[{_Key, Val} | _] -> {ok, Val}
    end.


-spec set(Key :: econfig_entry_key(), Val :: econfig_value(), t()) -> t().
set(Key, Val, #state{tid=Tid}=S) ->
    ets:insert(Tid, {Key, Val}),
    S.


-spec hash(Config :: t()) -> #{}.
hash(Config) ->
    hash(undefined, #{}, Config).


-spec hash(Data :: #{}, Config :: t()) -> #{}.
hash(Data, Config) ->
    hash(undefined, Data, Config).


hash(CurrentNS, Data, #state{tid=Tid}) ->
    ets:foldl(fun ({{App, Key}, Val}, Acc) ->
					  Acc2 = case App of
								 CurrentNS -> Acc#{ atom_to_list(Key) => Val };
								 _ -> Acc
							 end,
					  Acc2#{ render_key(App, Key) => Val }
			  end, Data, Tid).


to_list(#state{tid=Tid}) ->
    ets:foldl(fun ({{App, Key}, Val}, Acc) ->
					  [{render_key(App, Key), Val} | Acc]
			  end, [], Tid).

%%%
%%% Priv
%%%
-spec populate(econfig_config(), t()) -> t().
populate(C, S) ->
    lists:foreach(fun ({Key, Val}) ->
						  set(parse_key(Key), Val, S)
				  end, C),
    S.


parse_key(Key) ->
    case string:tokens(Key, ".") of
		[App | Rest] ->
			{list_to_atom(App), list_to_atom(string:join(Rest, "."))};
		_ ->
			throw({badentry, Key})
    end.


render_key(App, Key) ->
    string:join([atom_to_list(App), atom_to_list(Key)], ".").
