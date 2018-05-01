%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 32 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_read).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State=[]) ->
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData, undefined),
    UserId = case User of
		 _ when is_map(User) -> maps:get(user_id, User);
		 _ -> undefined
	     end,

    BlogName = cowboy_req:binding('blog_name', Req0),
    Blog = case blog_blog_mgr:get_blog_by_name(UserId, BlogName) of
	       {ok, B} -> B;
	       {error, not_found} -> undefined
	   end,
    {ok, HTML} = blog_read_page:render(SessionData#{blog=>Blog}),
    %% HTML = ["<pre>read ", BlogName, io_lib:format("~p", [Blog]), "</pre>"],
    Resp =  cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			     HTML, Req0),
    {ok, Resp, State}.
