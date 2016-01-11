%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_entry).

-include("econfig.hrl").
-include("econfig_log.hrl").

-type key() :: {atom(), atom()}.
-record(entry, {key                 :: key(),
				desc                :: string(),
				type                :: econfig_entry_type(),
				default             :: term(),
				select   = []       :: [econfig_dep:t()],
				depends  = []       :: [econfig_dep:t()],
				menu     = []       :: [econfig_dep:t()],
				choice   = []       :: [econfig_dep:t()],
				requires = []       :: [econfig_dep:t()],
				excludes = []       :: [econfig_dep:t()],
				opts                :: [econfig_entry_opt()]}).
-type t() :: #entry{}.

-export_type([t/0]).

-export([new/5,
		 new/2,
		 key/1,
		 desc/1,
		 type/1,
		 default/1,
		 menu/1,
		 deps/2,
		 eval/2]).

-spec new(Key :: key(), Desc :: string(), Type :: econfig_entry_type(), 
		  Dft :: term(), Opts :: [econfig_entry_opt()]) -> t().
new(Key, Desc, Type, Dft, Opts) ->
    #entry{key=Key, desc=Desc, type=Type, default=Dft, opts=Opts}.


-spec new(AppName :: atom(), Desc :: econfig_entry()) -> t().
new(App, {Key, Desc, Type, Default, Opts}) ->
    FullKey = econfig_utils:parse_key(App, Key),
    #entry{key=FullKey, desc=Desc, type=Type, default=Default, opts=Opts,
		   select=relation(App, select, Opts),
		   depends=relation(App, depends, Opts),
		   menu=relation(App, menu, Opts),
		   choice=relation(App, choice, Opts),
		   requires=relation(App, requires, Opts),
		   excludes=relation(App, excludes, Opts)}.


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


-spec menu(t()) -> boolean().
menu(#entry{menu=Menu}) ->
    Menu.


-spec deps(Type :: econfig_dep:dep_type(), Entry :: t()) -> [econfig_dep:t()].
deps(select,   #entry{select=D})   -> D;
deps(depends,  #entry{depends=D})  -> D;
deps(menu,     #entry{menu=D})     -> D;
deps(choice,   #entry{choice=D})   -> D;
deps(requires, #entry{requires=D}) -> D;
deps(excludes,  #entry{excludes=D}) -> D.


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
relation(App, Type, Opts) ->
    lists:foldl(fun (DepDesc, Acc) ->
						[ econfig_dep:new(App, DepDesc, Type) | Acc ]
				end, [], proplists:get_value(Type, Opts, [])).

