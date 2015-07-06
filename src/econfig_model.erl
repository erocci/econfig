%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc Creates and manipulate a configuration model
%%%
%%% @end
%%% Created :  6 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_model).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([new/0,
	 add_entries/3,
	 solve_deps/1]).

% pretty printer
-export([pp/1]).

-record model, {
	  ev           :: ets:tid(),        % {entry key, vertex} table
	  deps         :: ets:tid(),        % {dep key, value} table
	  graph        :: digraph:graph(),
	  root         :: digraph:vertex()
	 }.
-type t() :: #model{}.

-define(ROOT, {{econfig, root}, "", boolean, false, []}).

-spec new() -> t().
new() ->
    EV_Tid = ets:new(ev, []),
    Deps_Tid = ets:new(deps, []),
    G = digraph:new(),
    Root = digraph:add_vertex(G, ?ROOT),                  % root node connects to every entry
    #model{ev=EV_Tid, deps=Deps_Tid, graph=G, root=Root}.


-spec add_entries(AppName :: atom(), AppModel :: [econfig_entry()], t()) -> t().
add_entries(AppName, AppModel, Model) ->
    lists:foldl(fun ({Key, Desc, Type, Dft, Opts}, Acc) -> 
			add_entry({AppName, Key}, Desc, Type, Dft, Opts, Acc)
		end, Model, AppModel).


-spec solve_deps(t()) -> t().
solve_deps(#model{graph=G} = Model) ->
    lists:foldl(fun ({Key, _, _, _, Opts}, Acc) ->
			Deps = proplists:get_value(depends, Opts, []),
			add_deps(Key, Deps, Acc)
		end, Model, digraph:vertices(G)).

-spec pp(t()) -> ok.
pp(#model{root=_Root, graph=G}) ->
    lists:foldl(fun ({Key, _, _, _, _}, Acc) ->
			io:format("~b: ~p~n", [Acc, Key]),
			Acc+1
		end, 1, lists:reverse(digraph_utils:postorder(G))).

%%%
%%% Priv
%%%
add_entry({App, Key}, Desc, Type, Dft, Opts, #model{ev=Tid, graph=G, root=Root}=Model) ->
    V = digraph:add_vertex(G, {{App, Key}, Desc, Type, Dft, Opts}, {App, Key}),
    digraph:add_edge(G, V, Root),
    ets:insert(Tid, {{App, Key}, V}),
    Model.

add_deps({App, Key}, Deps, Model) ->
    lists:foldl(fun ({{DepApp, DepKey}, Val}, Acc) when is_atom(DepApp), is_atom(DepKey) ->
			add_dep({App, Key}, {DepApp, DepKey}, Val, Acc);
		    ({DepKey, Val}, Acc) when is_atom(Key) ->
			add_dep({App, Key}, {App, DepKey}, Val, Acc)
		end, Model, Deps).

add_dep(Entry, Dep, Val, #model{ev=EV, deps=Tid, graph=G}=Model) ->
    case {ets:lookup(EV, Entry), ets:lookup(EV, Dep)} of
	{[{_, V1}], [{_, V2}]} ->
	    case digraph:add_edge(G, V2, V1) of
		{error, {bad_edge, Path}} ->
		    throw({cycle, Path});
		{error, {bad_vertex, V}} ->
		    throw({badentry, V});
		_Edge ->
		    Model
	    end;
	{[], _} ->
	    throw({badentry, Entry});
	{_, []} ->
	    throw({badentry, Dep})
    end,
    ets:insert(Tid, {{Entry, Dep}, Val}),
    Model.
