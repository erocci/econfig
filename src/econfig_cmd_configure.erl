%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_configure).

-behaviour(econfig_cmd).

%% econfig_cmd behaviour API
-export([run/2]).

-cmd_name(configure).

run(State, _Opts) ->
    case econfig_state:configure(State) of
		{error, Err} ->
			econfig_cli:handle_error(Err);
		S2 ->
			econfig_state:store(S2)
    end.
