%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc This frontend always return default values (non interactive)
%%%
%%% @end
%%% Created :  8 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_ui_defconfig).

-behaviour(econfig_frontend).

% econfig_frontend behaviour
-export([start_link/2,
	 ask/3,
	 terminate/1]).

-record state, {}.

start_link(_, _) ->
    {ok, #state{}}.

ask({_, _, _Type, Default, _}=_E, _Config, Ref) ->
    {ok, Default, Ref}.

terminate(_Ref) ->
    ok.

%%%
%%% Priv
%%%
