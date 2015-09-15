%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_dep).

-include("econfig.hrl").

-type dep_key() :: {atom(), atom()}.
-type dep_type() :: select | depends | menu | choice | requires | excludes.

-record(dep, {key    :: dep_key(),
	      type   :: dep_type(),
	      val    :: econfig_entry_def()}).
-type t() :: #dep{}.

-export_type([t/0, dep_type/0]).

-export([new/3,
	 key/1,
	 val/1,
	 type/1,
	 match/2]).

-spec new(AppName :: atom(), Desc :: econfig_entry_dep(), Type :: dep_type()) -> t().
new(App, {DepKey, Val}, Type) when is_atom(DepKey), is_atom(App) ->
    #dep{key=econfig_utils:parse_key(App, DepKey), val=Val, type=Type};
new(App, DepKey, Type) ->
    new(App, {DepKey, '_'}, Type).


-spec key(t()) -> {atom(), atom()}.
key(#dep{key=Key}) ->
    Key.


-spec val(t()) -> econfig_entry_def().
val(#dep{val=V}) ->
    V.


-spec type(t()) -> econfig_entry_def().
type(#dep{type=T}) ->
    T.


-spec match(t(), econfig_entry_def()) -> boolean.
match(#dep{},                     undefined) -> false;
match(#dep{val='_'},              false) -> false;
match(#dep{val='_'},              _) -> true;
match(#dep{val=V},                V) -> true;
match(#dep{val={choice, Values}}, Val) -> lists:member(Val, Values);
match(#dep{val={lt, High}},       Val) when Val < High -> true;
match(#dep{val={gt, Low}},        Val) when Val > Low -> true;
match(#dep{},                     _) -> false.

%%%
%%% Priv
%%%
