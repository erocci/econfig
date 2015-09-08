%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig).

-behaviour(provider).

% Script mode
-export([main/1]).

% provider behaviour
-export([init/1, do/1, format_error/1]).

-include("econfig.hrl").
-include("econfig_log.hrl").

%%%
%%% Script mode API
%%%
main(Args) ->
    econfig_cli:run(Args).

%%%
%%% Provider API
%%%
init(State) ->
    S1 = econfig_rebar:init(State),
    ?debug("Init econfig plugins", []),
    {ok, S2} = econfig_prv_configure:init(S1),
    {ok, S3} = econfig_prv_template:init(S2),
    econfig_prv_clean:init(S3).

do(State) ->
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
