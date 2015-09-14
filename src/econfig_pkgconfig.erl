%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_pkgconfig).

-export([prefix/1]).

-spec prefix(Lib :: string()) -> string() | undefined.
prefix(Lib) ->
    variable(Lib, prefix).

%%%
%%% Priv
%%%
variable(Lib, Var) when is_list(Lib) ->
    Cmd = io_lib:format("pkg-config --variable=~s ~s 2> /dev/null", [Var, Lib]),
    case econfig_utils:cmd(Cmd) of
	{ok, Out} ->
	    string:strip(Out, right, $\n);
	{error, {status, 1}} ->
	    undefined;
	{error, Err} ->
	    throw(Err)
    end.
