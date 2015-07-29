%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(econfig_sup).

-include("econfig_log.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    Srv = #{id => econfig_srv,
	    start => {econfig_srv, start_link, []},
	    restart => permanent,
	    shutdown => 5000,
	    type => worker,
	    modules => [econfig_srv]},
    Log = #{id => econfig_log,
	    start => {econfig_log, start_link, []},
	    restart => permanent,
	    shutdown => 5000,
	    type => worker,
	    modules => [econfig_log]},
    {ok, {SupFlags, [Srv, Log]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
