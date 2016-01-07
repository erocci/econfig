%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc This frontend always return default values (non interactive)
%%%
%%% @end
%%% Created :  8 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_ui_defconfig).

-include("econfig_log.hrl").

-behaviour(econfig_frontend).

% econfig_frontend behaviour
-export([start_link/2,
	 run/3,
	 terminate/1,
	 format_error/1]).

-record state, {}.

start_link(_, _) ->
    {ok, #state{}}.

run(Model, Config, Ref) ->
    C1 = lists:foldl(fun (Entry, C0) ->
			     Key = econfig_entry:key(Entry),
			     Default = econfig_entry:default(Entry),
			     econfig_config:set(Key, Default, C0)
		     end, Config, econfig_model:entries(Model)),
    {ok, C1, Ref}.

terminate(_Ref) ->
    ok.

format_error(Err) ->
    ?error("~p", [Err]).

%%%
%%% Priv
%%%
