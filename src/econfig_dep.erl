%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 14 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_dep).

-include("econfig.hrl").

-record(dep, {key    :: {atom(), atom()},
	      val    :: econfig_entry_def()}).
-type t() :: #dep{}.

-export_type([t/0]).

-export([new/2,
	 key/1,
	 val/1,
	 match/2]).

-spec new(Desc :: econfig_entry_dep(), AppName :: atom()) -> t().
new({{DepApp, DepKey}, Val}, _) when is_atom(DepApp), is_atom(DepKey) -> 
    #dep{key={DepApp, DepKey}, val=Val};
new({{DepKey}, Val}, App) when is_atom(DepKey), is_atom(App) ->
    #dep{key={App, DepKey}, val=Val};
new({{DepApp, DepKey}}, _) when is_atom(DepApp), is_atom(DepKey) ->
    #dep{key={DepApp, DepKey}, val='_'};
new({DepKey}, App) when is_atom(DepKey), is_atom(App) ->
    #dep{key={App, DepKey}, val='_'}.


-spec key(t()) -> {atom(), atom()}.
key(#dep{key=Key}) ->
    Key.


-spec val(t()) -> econfig_entry_def().
val(#dep{val=V}) ->
    V.


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
