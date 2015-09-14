%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_entry).

-include("econfig.hrl").

-record(entry, {key      :: {atom(), atom()},
		desc     :: string(),
		type     :: econfig_entry_type(),
		default  :: term(),
		deps     :: [dep()],
		opts     :: [econfig_entry_opt()]}).
-type t() :: #entry{}.

-record(dep, {key    :: {atom, atom()},
	      val    :: econfig_entry_def(),
	      edge   :: digraph:edge()}).
-type dep() :: #dep{}.

-export_type([t/0, dep/0]).

-export([new/1,
	 new/2,
	 key/1,
	 desc/1,
	 type/1,
	 default/1,
	 deps/1,
	 deps/2,
	 eval/2]).

-spec new(Desc :: econfig_entry()) -> t().
new({{App, Key}, Desc, Type, Default, Opts}) ->
    #entry{key={App, Key}, desc=Desc, type=Type, default=Default, deps=[], opts=Opts}.


-spec new(AppName :: atom(), Desc :: econfig_entry()) -> t().
new(App, {Key, Desc, Type, Default, Opts}) ->
    Deps = lists:foldl(fun (DepDesc, Acc) ->
			       [ econfig_dep:new(DepDesc, App) | Acc ]
		       end, [], proplists:get_value(depends, Opts, [])),
    #entry{key={App, Key}, desc=Desc, type=Type, default=Default, deps=Deps, opts=Opts}.


-spec key(Entry :: t()) -> {atom(), atom()}.
key(#entry{key=Key}) ->
    Key.


-spec desc(Entry :: t()) -> {atom(), atom()}.
desc(#entry{desc=Desc}) ->
    Desc.


-spec default(Entry :: t()) -> {atom(), atom()}.
default(#entry{default=Default}) ->
    Default.


-spec type(t()) -> econfig_entry_type().
type(#entry{type=T}) ->
    T.


-spec deps(Entry :: t()) -> [dep()].
deps(#entry{deps=Deps}) ->
    Deps.


-spec deps(Deps :: [dep()], Entry :: t()) -> t().
deps(Deps, #entry{}=Entry) when is_list(Deps) ->
    Entry#entry{deps=Deps}.


-spec eval(fun(), t()) -> econfig_value() | undefined.
eval(Fun, #entry{opts=Opts}=Entry) ->
    case proplists:get_value(call, Opts, undefined) of
	undefined -> 
	    Fun(Entry);
	{M, F, A} ->
	    apply(M, F, A)
    end.

%%%
%%% Priv
%%%
