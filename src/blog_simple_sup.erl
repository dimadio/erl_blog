%%%-------------------------------------------------------------------
%% @doc blog_simle top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blog_simple_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, 
		   [
		    child(session_mgr, blog_session_mgr),
		    child(user_mgr, blog_user_mgr),
		    child(blog_mgr, blog_blog_mgr),
		    child(entry_mgr, blog_entry_mgr)
		   ]} }.

%%====================================================================
%% Internal functions
%%====================================================================



child(Name, Mod)->
	#{id => Name,
	  start => {Mod, start_link, []},
	  restart => permanent,
	  shutdown => 5000,
	  type => worker,
	  modules => [Mod]}.
