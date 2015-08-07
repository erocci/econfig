%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(econfig_srv).

-behaviour(gen_server).

-include("econfig.hrl").
-include("econfig_log.hrl").

%% API
-export([start_link/0,
	 load/1,
	 models/1,
	 print/0,
	 configure/0,
	 get/3,
	 set/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  model :: econfig_model:t(),
	  config :: econfig_config:t(),
	  frontend :: econfig_frontend:t()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load(Dirs) ->
    gen_server:call(?SERVER, {load, Dirs}).

models(Models) ->
    gen_server:call(?SERVER, {models, Models}).

print() ->
    gen_server:call(?SERVER, print).

-spec configure() -> {ok, econfig_config:t()} | {error, term()}.
configure() ->
    gen_server:call(?SERVER, configure, infinity).

get(App, Name, Default) ->
    gen_server:call(?SERVER, {get, App, Name, Default}).

set(App, Name, Val) ->
    gen_server:call(?SERVER, {set, App, Name, Val}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{model = econfig_model:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, App, Name, Default}, _From, #state{config=C}=State) ->
    case econfig_config:lookup({App, Name}, C) of
	undefined -> 
	    {reply, Default, State};
	{ok, Val} -> 
	    {reply, Val, State}
    end;

handle_call({set, App, Name, Val}, _From, #state{config=C}=State) ->
    try econfig_config:set({App, Name}, Val, C) of
	ok ->
	    {reply, ok, State}
    catch _:Err ->
	    {reply, {error, Err}, State}
    end;

handle_call({load, Dirs}, _From, State) ->
    Filenames = [ filename:join([Dir, "Econfig"]) || Dir <- Dirs],
    case load_models(Filenames) of
	{ok, Model} ->
	    {reply, ok, State#state{model=Model}};
	{error, _} = Err ->
	    {reply, Err, State}
    end;

handle_call({models, Models}, _From, State) ->
    try compile(Models) of
	ConfigModel ->
	    {reply, ok, State#state{model=ConfigModel}}
    catch throw:Err ->
	    {reply, {error, Err}, State}
    end;

handle_call(print, _From, #state{model=Model}=State) ->
    econfig_model:pp(Model),
    {reply, ok, State};

handle_call(configure, _From, #state{model=Model}=State) ->
    C = econfig_config:new(Model),
    F = econfig_frontend:new(Model),
    case econfig_frontend:run(C, F) of
	{ok, C1} ->
	    {reply, {ok, C1}, State#state{config=C1, frontend=F}};
	{error, _} = Err ->
	    {reply, Err, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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


compile(Entries) ->
    M0 = econfig_model:new(),
    Model = lists:foldl(fun ({AppName, AppEntries}, Acc) ->
				econfig_model:add_entries(AppName, AppEntries, Acc)
			end, M0, Entries),
    econfig_model:solve_deps(Model).
