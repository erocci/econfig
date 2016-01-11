%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_print).

-include("econfig_log.hrl").

-behaviour(econfig_cmd).

-cmd_name(print).
-cmd_desc("Print current configuration").

%% econfig_cmd behaviour API
-export([run/2,
		 usage/0]).

-define(output, [erlang, sh, value]).
-define(output_str, 
		io_lib:format("(~s)", [string:join([ atom_to_list(F) || F <- ?output], "|")])).
-define(argspec, [
				  {help,      $h, "help",      undefined,           "Show this help"}
				 ,{out,       $o, "out",       {string, "erlang"},  "Output format " ++ ?output_str}
				 ]).

run(State, Args) ->
	case getopt:parse(?argspec, Args) of
		{ok, {Opts, Keys}} ->
			case proplists:get_bool(help, Opts) of
				true -> 
					usage(),
					erlang:halt(0);
				false ->
					S2 = econfig_state:load(State),
					run2(Keys, econfig_state:config(S2), Opts)
			end;
		_ ->
			usage(),
			erlang:halt(1)
	end.

usage() ->
	econfig_cmd:usage(print, ?argspec, "[<key> ...]", []),
	ok.

%%%
%%% Priv
%%%
run2([], Config, Opts) ->
	pp(list_to_atom(proplists:get_value(out, Opts, erlang)), econfig_config:to_list(Config));

run2(Keys, Config, Opts) ->
	C2 = lists:filter(fun ({Key, _Val}) ->
							  lists:member(Key, Keys)
					  end, econfig_config:to_list(Config)),
	pp(list_to_atom(proplists:get_value(out, Opts, erlang)), C2).


pp(erlang, Config) ->
	lists:foreach(fun ({Key, Val}) ->
						  io:format("{'~s', ~p}.~n", [Key, Val])
				  end, Config);

pp(sh, Config) ->
	lists:foreach(fun ({Key, Val}) ->
						  io:format("~s=~s~n", [render_sh_key(Key), render_sh_val(Val)])
				  end, Config);

pp(value, Config) ->
	lists:foreach(fun ({_Key, Val}) ->
						  io:format("~s~n", [render_value(Val)])
				  end, Config);

pp(Format, _) ->
	?error("Invalid output format: ~s", [Format]),
	usage(),
	erlang:halt(1).

render_sh_key(Key) -> re:replace(string:to_upper(Key), "\\.", "_", [global, {return,list}]).

render_sh_val(true)  -> "1";
render_sh_val(false) -> "0";
render_sh_val(V) when is_atom(V) -> atom_to_list(V);
render_sh_val(V) when is_integer(V) -> integer_to_list(V);
render_sh_val(V) -> io_lib:format("~p", [V]).


render_value(true)  -> "1";
render_value(false) -> "0";
render_value(V) when is_atom(V) -> atom_to_list(V);
render_value(V) when is_integer(V) -> integer_to_list(V);
render_value(V) when is_list(V) -> V;
render_value(V) -> io_lib:format("~p", [V]).
