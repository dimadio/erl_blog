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
    {ok, Entries} = case Blog of 
		  undefined -> {ok, []};
		  _ ->
		      blog_entry_mgr:get_entries(maps:get(blog_id,Blog), [])
	      end,
    lager:info("Entries: ~p", [Entries]),
    {ok, HTML} = blog_read_page:render(SessionData#{blog=>Blog, entries => Entries}),
    %% HTML = ["<pre>read ", BlogName, io_lib:format("~p", [Blog]), "</pre>"],
    Resp =  cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			     HTML, Req0),
    {ok, Resp, State};




init(Req0 = #{method := <<"POST">>}, State=[]) ->
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
    BlogId = maps:get(blog_id,Blog),

    {ok, BodyList, Req1} = cowboy_req:read_urlencoded_body(Req0),
    Body = maps:from_list(BodyList),
    lager:info("POST  body :~p", [Body]),
    Title = maps:get(<<"title">>, Body),
    Content = maps:get(<<"content">>, Body),
    Tags = [],
    {ok, EntryId} = blog_entry_mgr:add_entry(BlogId, Title, Content, Tags),
    lager:info("Added entry ~p", [EntryId]),

    Resp = cowboy_req:reply(302,
			    #{<<"content-type">> => <<"text/plain">>,
			      <<"location">> => << "/@", BlogName/binary >>},
			    <<>>,
			    Req1),
    {ok, Resp, State};

init(Req0 = #{method := Method}, State) ->
    
    lager:info("Method = ~p", [Method]),
    {ok, Req0, State}.
