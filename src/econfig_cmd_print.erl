%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_cmd_print).

-behaviour(econfig_cmd).

-cmd_name(print).

%% econfig_cmd behaviour API
-export([run/2]).

run(State, _Opts) ->
    econfig_config:pp(econfig_state:config(State)),
    ok.
