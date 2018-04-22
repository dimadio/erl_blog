%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_simple_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	blog_tools:init_db(),

	Dispatch = cowboy_router:compile([							
									  {'_', [
											 {"/s/[...]", cowboy_static, 
											  {priv_dir, blog_simple, "static"}},
											 {"/login", login_handler, []},
											 {"/logout", logout_handler, []},
											 {'_', root_handler, []}]}
									 ]),

	Middlewares = [
				   blog_mid_access_log,
				   cowboy_router,
				   blog_mid_session,
				   blog_mid_login,
				   cowboy_handler], 
	PublicPages = [
				   <<"/">>,
				   <<"/login">>,
				   <<"/logout">>,
				   <<"/about">>
				  ],

    {ok, _} = cowboy:start_clear(http_listener,
								 [{port, 9091}],
								 #{env => #{dispatch => Dispatch,
											public_pages => PublicPages},
								   middlewares => Middlewares}
								),
    blog_simple_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
