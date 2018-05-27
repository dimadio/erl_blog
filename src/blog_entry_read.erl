%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 17 May 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_entry_read).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State=[display]) ->
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData, undefined),
    UserId = case User of
		 _ when is_map(User) -> maps:get(user_id, User);
		 _ -> undefined
	     end,

    BlogName = cowboy_req:binding('blog_name', Req0),
    EntryTitle = cowboy_req:binding('entry_title', Req0),

    Blog = case blog_blog_mgr:get_blog_by_name(UserId, BlogName) of
	       {ok, B} -> B;
	       {error, not_found} -> #{}
	   end,

    Entry = case blog_entry_mgr:get_entry(maps:get(blog_id, Blog, undefined), EntryTitle) of
	       {ok, E} -> E;
	       {error, not_found} -> undefined
	   end,

    {ok, HTML} = blog_read_entry:render(SessionData#{blog=>Blog, entry => Entry, blog => Blog}),
    
    Resp =  cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			     HTML, Req0),
    {ok, Resp, State};



init(Req0 = #{method := <<"GET">>}, State=[action]) ->
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData, undefined),
    UserId = case User of
		 _ when is_map(User) -> maps:get(user_id, User);
		 _ -> undefined
	     end,

    BlogName = cowboy_req:binding('blog_name', Req0),
    EntryTitle = cowboy_req:binding('entry_title', Req0),
    Action = cowboy_req:binding('action', Req0),

    Blog = case blog_blog_mgr:get_blog_by_name(UserId, BlogName) of
	       {ok, B} -> B;
	       {error, not_found} -> #{}
	   end,

    Entry = case blog_entry_mgr:get_entry(maps:get(blog_id, Blog, undefined), EntryTitle) of
	       {ok, E} -> E;
	       {error, not_found} -> undefined
	   end,

    Resp = execute_action(Blog, Entry, Action, Req0),
    {ok, Resp, State}.



execute_action(Blog, Entry, <<"edit">>, Req) ->
    Resp =  cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			     <<"edit">>, Req).
