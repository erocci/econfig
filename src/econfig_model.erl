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
	 entries/3,
	 entries/1,
	 compile/1,
	 eval/4,
	 is_enable/3]).

% pretty printer
-export([pp/1]).

-record model, {
	  graph :: digraph:graph()
	 }.

-type t() :: #model{}.

-export_type([t/0]).

-define(root_key, {econfig, root}).
-define(root, {"", "", boolean, false, []}).

-spec new() -> t().
new() ->
    G = digraph:new([acyclic]),
    Root = econfig_entry:new(?root_key, "", boolean, false, []),
    digraph:add_vertex(G, ?root_key, Root),                          % root node connects to every entry
    #model{graph=G}.


-spec get_entry(econfig_entry_key(), t()) -> econfig_entry:t() | undefined.
get_entry(Key, #model{graph=G}) ->
    case digraph:vertex(G, Key) of
	{Key, Entry} -> Entry;
	_ -> undefined
    end.


-spec entries(AppName :: binary() | string() | atom(), AppModel :: [econfig_entry()], t()) -> t().
entries(AppName, AppModel, Model) when is_binary(AppName) ->
    entries(list_to_atom(binary_to_list(AppName)), AppModel, Model);
entries(AppName, AppModel, Model) when is_list(AppName) ->
    entries(list_to_atom(AppName), AppModel, Model);
entries(AppName, AppModel, Model) ->
    lists:foldl(fun (EntryDesc, Acc) ->
			Entry = econfig_entry:new(AppName, EntryDesc),
			add_entry(Entry, Acc)
		end, Model, AppModel).


-spec entries(t()) -> [econfig_entry:t()].
entries(#model{}=Model) ->
    lists:foldl(fun (?root_key, Acc) ->
			Acc;
		    (Key, Acc) ->
			{_, Entry} = digraph:vertex(Model#model.graph, Key),
			[ Entry | Acc]
		end, [], digraph_utils:reaching([?root_key], Model#model.graph)).


-spec compile(t()) -> t().
compile(#model{}=Model) ->
    M2 = loop_deps(select, Model),
    M3 = loop_deps(depends, M2),
    M4 = loop_deps(menu, M3),
    M5 = loop_deps(choice, M4),
    M6 = loop_deps(requires, M5),
    loop_deps(excludes, M6).


-spec pp(t()) -> ok.
pp(#model{}=Model) ->
    lists:foldl(fun (Key, Acc) ->
			io:format("~b: ~p~n", [Acc, Key]),
			Acc+1
		end, 1, lists:reverse(digraph_utils:reaching([?root_key], Model#model.graph))).


-spec eval(Entry :: econfig_entry:t(), 
	   Fun :: fun(), 
	   Config :: econfig_config:t(), 
	   Model :: econfig_model:t()) -> econfig_config:t().
eval(Entry, Fun, Config, Model) ->
    Val = case is_enable(Entry, Config, Model) of
	      true ->
		  econfig_entry:eval(Fun, Entry);
	      false ->
		  disable(econfig_entry:type(Entry))
	  end,
    set(Entry, Val, Config, Model).

-spec is_enable(Entry :: econfig_entry:t(), Config :: entry_config:t(), t()) -> boolean().
is_enable(Entry, Config, Model) ->
    % TODO: check other relations
    Deps = [ Dep || Dep <- deps(Entry, Model), econfig_dep:type(Dep) =:= depends ],
    is_enable_(Deps, Config).

%%%
%%% Priv
%%%
set(Entry, Val, Config, Model) ->
    C0 = econfig_config:set(econfig_entry:key(Entry), Val, Config),
    select([Entry], C0, Model).


select([], Config, _) ->
    Config;
select([Entry | Tail], Config, Model) ->
    {E2, C2} = lists:foldl(fun (Dep, {E0, C0}) ->
				   DepKey = econfig_dep:key(Dep),
				   DepEntry = entry(DepKey, Model),
				   Val = select_value(DepEntry, econfig_dep:val(Dep)),
				   C1 = econfig_config:set(DepKey, Val, C0),
				   E1 = [ DepEntry | E0 ],
				   {E1, C1}
			   end, {Tail, Config}, econfig_entry:deps(select, Entry)),
    select(E2, C2, Model).


entry(Key, #model{graph=G}) ->
    {_, Entry} = digraph:vertex(G, Key),
    Entry.


select_value(Entry, '_') ->
    case econfig_entry:type(Entry) of
	boolean -> 
	    true;
	_ ->
	    econfig_entry:default(Entry)
    end;
select_value(_, Val) ->
    Val.
    

deps(Entry, #model{graph=G}) ->
    Key = econfig_entry:key(Entry),
    lists:foldl(fun (E, Acc) ->
			{_, _, _, Dep} = digraph:edge(G, E),
			[ Dep | Acc ]
		end, [], digraph:in_edges(G, Key)).


add_entry(Entry, #model{graph=G}=Model) ->
    V = digraph:add_vertex(G, econfig_entry:key(Entry), Entry),
    digraph:add_edge(G, V, ?root_key, undefined),
    Model.


loop_deps(Type, Model) ->
    lists:foldl(fun (Key, Acc) ->
			{_, Entry} = digraph:vertex(Acc#model.graph, Key),
			lists:foldl(fun (Dep, Acc2) ->
					    add_dep(Entry, Dep, Acc2)
				    end, Acc, econfig_entry:deps(Type, Entry))
		end, Model, digraph:vertices(Model#model.graph)).


add_dep(Entry, Dep, #model{graph=G}=Model) ->
    Key = econfig_entry:key(Entry),
    DepKey = econfig_dep:key(Dep),
    case digraph:add_edge(G, DepKey, Key, Dep) of
	{error, {bad_edge, Path}} ->
	    throw({cycle, Path});
	{error, {bad_vertex, V}} ->
	    throw({badentry, V});
	_Edge ->
	    Model
    end.	


is_enable_([], _) -> 
    true;
is_enable_([ Dep | Tail ], Config) ->
    Key = econfig_dep:key(Dep),
    case econfig_config:lookup(Key, Config) of
	{ok, Val} -> 
	    case econfig_dep:match(Dep, Val) of
		true -> is_enable_(Tail, Config);
		false -> false
	    end;
	undefined -> false
    end.


disable(boolean) -> false;
disable(_) -> undefined.
