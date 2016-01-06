%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig).

% Script mode
-export([main/1]).

-include("econfig.hrl").
-include("econfig_log.hrl").

%%%
%%% Script mode API
%%%
main(Args) ->
    econfig_cli:run(Args).
