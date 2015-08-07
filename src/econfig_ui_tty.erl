%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_ui_tty).

-behaviour(econfig_frontend).

% econfig_frontend behaviour
-export([start_link/2,
	 ask/3,
	 terminate/1]).

-record state, {model :: econfig_model:t()}.

start_link(Model, _) ->
    {ok, #state{model=Model}}.

ask({_, _, Type, Default, _}=E, _Config, Ref) ->
    case io:get_line(prompt(E)) of
	{error, Err} -> throw(Err);
	eof -> throw({invalid_input, eof});
	Data -> 
	    case cast(strip(Data), Type, Default) of
		{error, {invalid_input, V}} ->
		    io:format("E: Invalid value: ~p~n", [V]),
		    ask(E, _Config, Ref);
		{error, {invalid_type, T}} ->
		    throw({invalid_type, T});
		V -> 
		    {ok, V, Ref}
	    end
    end.

terminate(_Ref) ->
    ok.

%%%
%%% Priv
%%%
prompt({{App, Key}, Desc, Type, Default, _}) ->
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
