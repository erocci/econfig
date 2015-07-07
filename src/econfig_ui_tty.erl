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
	    Val = cast(Data, Type, Default),
	    {ok, Val, Ref}
    end.

terminate(_Ref) ->
    ok.

%%%
%%% Priv
%%%
prompt({{App, Key}, Desc, _, _, _}) ->
    io_lib:format("(~p/~p) ~s", [App, Key, Desc]).

cast([$\n], _Type, Default) ->
    Default;
cast(Data, _Type, _Default) ->
    Data.
