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
    User = maps:get(user, SessionData),
    UserId = maps:get(user_id, User),

    BlogName = cowboy_req:binding('blog_name', Req0),
    HTML = ["<pre>read ", BlogName, "</pre>"],
    Resp =  cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			     HTML, Req0),
    {ok, Resp, State}.
