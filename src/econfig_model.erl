%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc Creates and manipulate a configuration model
%%%
%%% @end
%%% Created :  6 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_model).

-include("econfig.hrl").
-include("econfig_log.hrl").

% API
-export([new/0,
	 get_entry/2,
	 add_entries/3,
	 solve_deps/1,
	 entries/1,
	 deps/2]).

% pretty printer
-export([pp/1]).

-record model, {
	  graph :: digraph:graph(),
	  deps  :: ets:tid()
	 }.

-type t() :: #model{}.

-export_type([t/0]).

-define(root_key, {econfig, root}).
-define(root, {?root_key, "", boolean, false, []}).

-spec new() -> t().
new() ->
    G = digraph:new([acyclic]),
    digraph:add_vertex(G, ?root_key, econfig_entry:new(?root)),          % root node connects to every entry
    Tid = ets:new(?MODULE, []),
    #model{graph=G, deps=Tid}.


-spec get_entry(econfig_entry_key(), t()) -> econfig_entry:t() | undefined.
get_entry(Key, #model{graph=G}) ->
    case digraph:vertex(G, Key) of
	{Key, Entry} -> Entry;
	_ -> undefined
    end.


-spec add_entries(AppName :: binary() | string() | atom(), AppModel :: [econfig_entry()], t()) -> t().
add_entries(AppName, AppModel, Model) when is_binary(AppName) ->
    add_entries(list_to_atom(binary_to_list(AppName)), AppModel, Model);
add_entries(AppName, AppModel, Model) when is_list(AppName) ->
    add_entries(list_to_atom(AppName), AppModel, Model);
add_entries(AppName, AppModel, Model) ->
    lists:foldl(fun (EntryDesc, Acc) ->
			Entry = econfig_entry:new(AppName, EntryDesc),
			add_entry(Entry, Acc)
		end, Model, AppModel).


-spec solve_deps(t()) -> t().
solve_deps(#model{graph=G}=Model) ->
    lists:foldl(fun (Key, Acc) ->
			{_, Entry} = digraph:vertex(Acc#model.graph, Key),
			lists:foldl(fun (Dep, Acc2) ->
					    add_dep(Key, Dep, Acc2)
				    end, Acc, econfig_entry:deps(Entry))
		end, Model, digraph:vertices(G)).


-spec pp(t()) -> ok.
pp(#model{}=Model) ->
    lists:foldl(fun (Key, Acc) ->
			io:format("~b: ~p~n", [Acc, Key]),
			Acc+1
		end, 1, lists:reverse(digraph_utils:reaching([?root_key], Model#model.graph))).


-spec entries(t()) -> [econfig_entry:t()].
entries(#model{}=Model) ->
    lists:foldl(fun (?root_key, Acc) ->
			Acc;
		    (Key, Acc) ->
			{_, Entry} = digraph:vertex(Model#model.graph, Key),
			[ Entry | Acc]
		end, [], digraph_utils:reaching([?root_key], Model#model.graph)).


-spec deps(econfig_entry(), t()) -> [econfig_entry:dep()].
deps(Entry, #model{graph=G, deps=Tid}) ->
    Key = econfig_entry:key(Entry),
    lists:foldl(fun (DepKey, Acc) when DepKey =:= Key -> 
			Acc;
		    (DepKey, Acc) ->
			[ {_Key, Dep} ] = ets:lookup(Tid, {DepKey, Key}),
			[ Dep | Acc ]
		end, [], digraph_utils:reaching([Key], G)).

%%%
%%% Priv
%%%
add_entry(Entry, #model{graph=G}=Model) ->
    V = digraph:add_vertex(G, econfig_entry:key(Entry), Entry),
    digraph:add_edge(G, V, ?root_key, undefined),
    Model.

add_dep(Key, Dep, #model{graph=G, deps=Tid}=Model) ->
    DepKey = econfig_dep:key(Dep),
    case digraph:add_edge(G, DepKey, Key) of
	{error, {bad_edge, Path}} ->
	    throw({cycle, Path});
	{error, {bad_vertex, V}} ->
	    throw({badentry, V});
	_Edge ->
	    ets:insert(Tid, {{DepKey, Key}, Dep}),
	    Model
    end.
