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
-export([update_dispatch/0]).

%%====================================================================
%% API
%%====================================================================

update_dispatch()->
    Dispatch = build_dispatch(),
    cowboy:set_env(http_listener, dispatch,Dispatch).

build_dispatch()->
    cowboy_router:compile([
			   {'_', [
				  {"/s/[...]", cowboy_static, 
				   {priv_dir, blog_simple, "static"}},
				  {"/login", login_handler, []},
				  {"/logout", logout_handler, []},
				  {"/blogs[/]", blogs_manager, [list]},
				  {"/blogs/add[/]", blogs_manager, [add]},
				  {"/blog/:blog_action", blogs_manager, [action]},
				  {"/:blog_name", [{blog_name, fun blog_name_filter/2}],blog_read, []},
				  {"/:blog_name/:entry_title", [{blog_name, fun blog_name_filter/2}],blog_entry_read, []},
				  {"/", root_handler, []}]}
			  ]).

start(_StartType, _StartArgs) ->
    blog_tools:init_db(),

    Dispatch = build_dispatch(),

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
blog_name_filter(forward, << "@", BlogName/binary>> )->
    {ok, BlogName};
blog_name_filter(forward,_) -> {error, no_blog_name};

blog_name_filter(reverse, Value = << "@", BlogName/binary>> ) -> {ok, Value};
blog_name_filter(reverse, Value ) -> {ok, << "@", Value/binary>>};

blog_name_filter(format_error, {no_blog_name, Value} ) ->
    io_lib:format("The value ~p is not a blog name.", [Value]);
blog_name_filter(Command, Value) ->
    lager:info("blog_name_filter called with ~p: ~p", [Command, Value]),
    {error, not_found}.
