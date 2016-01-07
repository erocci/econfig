%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_state).

-include("econfig.hrl").
-include("econfig_log.hrl").

-export([new/1,
	 load/1,
	 config/1,
	 basedir/1,
	 models/2,
	 store/1,
	 parse_models/2,
	 configure/1]).

-record(state, {
	  basedir  :: filename:file(),
	  model    :: econfig_model:t(),
	  config   :: econfig_config:t(),
	  frontend :: econfig_frontend:t()}
       ).

-type t() :: #state{}.

-export_type([t/0]).


-spec new(Basedir :: filename:file()) -> t().
new(Basedir) ->
    #state{
       basedir = Basedir,
       model = econfig_model:new(),
       config = econfig_config:new()
      }.


-spec load(t()) -> t() | {error, term()}.
load(#state{config=C, basedir=Basedir}=S) ->
    Filename = filename:join([Basedir, ".econfig"]),
    case econfig_config:load(Filename, C) of
	{error, _} = Err -> Err;
	C2 -> S#state{config=C2}
    end.
		  

-spec config(t()) -> econfig_config:t().
config(#state{config=C}) ->
    C.

-spec basedir(t()) -> filename:file().
basedir(#state{basedir=D}) ->
    D.

-spec models(Models :: econfig_model:t(), t()) -> t() | {error, term()}.
models(Models, S) ->
    try compile(Models) of
	ConfigModel ->
	    S#state{model=ConfigModel}
    catch throw:Err ->
	    {error, Err}
    end.

-spec parse_models(Dirs :: [filename:file()], t()) -> t() | {error, term()}.
parse_models(Dirs, S) ->
    Filenames = [ filename:join([Dir, "Econfig"]) || Dir <- Dirs],
    case load_models(Filenames) of
	{ok, Model} ->
	    S#state{model=Model};
	{error, _} = Err -> 
	    Err
    end.

-spec configure(t()) -> t() | {error, term()}.
configure(#state{config=C, model=Model}=S) ->
    F = econfig_frontend:new(Model),
    case econfig_frontend:run(C, F) of
	{ok, C1, F1} ->
	    S#state{config=C1, frontend=F1};
	{error, _} = Err ->
	    Err
    end.

-spec store(t()) -> ok | {error, term()}.
store(#state{config=C, basedir=B}) ->
    Filename = filename:join([B, ".econfig"]),
    econfig_config:store(Filename, C).

%%%
%%% Priv
%%%
compile(Entries) ->
    M0 = econfig_model:new(),
    Model = lists:foldl(fun ({AppName, AppEntries}, Acc) ->
				econfig_model:entries(AppName, AppEntries, Acc)
			end, M0, Entries),
    econfig_model:compile(Model).

load_models(Filenames) ->
    AppEntries = lists:foldl(fun (Filename, Acc) ->
				     case load_model(Filename) of
					 {ok, {App, Model}} ->
					     ?debug("Loaded model from ~s~n", [Filename]),
					     [{App, Model} | Acc];
					 {error, enoent} ->
					     Acc;
					 {error, {Line, erl_parse, ParseErr}} ->
					     ?warn("Error parsing ~s, line ~p: ~s", [Filename, Line, ParseErr]),
					     Acc;
					 {error, _} = Err ->
					     throw(Err)
				     end
			     end, [], Filenames),
    ConfigModel = compile(AppEntries),
    {ok, ConfigModel}.

load_model(Filename) ->
    App = filename:basename(filename:dirname(Filename)),
    case file:consult(Filename) of
	{ok, Entries} ->
	    {ok, {list_to_atom(App), Entries}};
	{error, _} = Err ->
	    Err
    end.
