%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_ui_tty).

-include("econfig.hrl").
-include("econfig_log.hrl").

-behaviour(econfig_frontend).

% econfig_frontend behaviour
-export([start_link/2,
	 run/3,
	 terminate/1]).

-record state, {model :: econfig_model:t()}.

start_link(Model, _) ->
    {ok, #state{model=Model}}.

run(Model, Config, Ref) ->
    Entries = lists:filter(fun (Entry) ->
				   case econfig_config:lookup(econfig_entry:key(Entry), Config) of
				       {ok, _} -> false;
				       undefined -> true
				   end
			   end, econfig_model:entries(Model)),
    C1 = lists:foldl(fun (Entry, C0) ->
			     econfig_model:eval(Entry, 
						fun (E) -> ask(E, C0) end,
						C0, Model)
		     end, Config, Entries),
    {ok, C1, Ref}.

terminate(_Ref) ->
    ok.

%%%
%%% Priv
%%%
ask(Entry, Config) ->
    case io:get_line(prompt(Entry)) of
	{error, Err} -> throw(Err);
	eof -> throw({invalid_input, eof});
	Data -> 
	    Type = econfig_entry:type(Entry),
	    Default = econfig_entry:default(Entry),
	    case cast(strip(Data), Type, Default) of
		{error, {invalid_input, V}} ->
		    io:format("E: Invalid value: ~p~n", [V]),
		    ask(Entry, Config);
		{error, {invalid_type, T}} ->
		    throw({invalid_type, T});
		V -> 
		    V
	    end
    end.

prompt(E) ->
    {App, Key} = econfig_entry:key(E),
    Desc = econfig_entry:desc(E),
    Type = econfig_entry:type(E),
    Default = econfig_entry:default(E),
    io_lib:format("(~p.~p) ~s [~s] : ", [App, Key, Desc, prompt_default(Type, Default)]).

prompt_default(boolean, true) -> "Y/n";
prompt_default(boolean, false) -> "n/Y";

prompt_default(string, Dft) -> "str, default: \"" ++ Dft ++ "\"";

prompt_default({enum, Opts}, Dft) ->
    string:join(lists:map(fun (Opt) when Opt =:= Dft ->
				  string:to_upper(atom_to_list(Dft));
			      (Opt) ->
				  atom_to_list(Opt)
			  end, Opts), "|");

prompt_default(integer, Dft) -> 
    io_lib:format("int, default: ~b", [Dft]);

prompt_default({range, Min, Max}, Dft) -> 
    io_lib:format("~b-~b, default: ~b", [Min, Max, Dft]).
    

cast([], _Type, Default) -> Default;

cast("y", boolean, _) -> true;
cast("Y", boolean, _) -> true;
cast("n", boolean, _) -> false;
cast("N", boolean, _) -> false;

cast(V, string, _) -> V;

cast(V, {enum, Opts}, _) -> cast_enum(V, Opts);

cast(V, integer, _) ->
    try list_to_integer(V) of
	I -> I
    catch _:badarg ->
	    {error, {invalid_input, V}}
    end;

cast(V, {range, Min, Max}, _) ->
    try list_to_integer(V) of
	I when I >= Min, I =< Max -> I;
	_ -> {error, {invalid_input, V}}
    catch _:badarg ->
	    {error, {invalid_input, V}}
    end;
    
cast(_, Type, _) ->
    {error, {invalid_type, Type}}.


cast_enum(V, []) -> {error, {invalid_input, V}};
cast_enum(V, [Opt | Opts]) ->
    case atom_to_list(Opt) of
	V -> Opt;
	_ -> cast_enum(V, Opts)
    end.


strip(Data) ->
    string:strip(string:strip(Data, both, $\n), both).
