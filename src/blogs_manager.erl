%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blogs_manager).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State=[list]) ->
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData),
    UserId = maps:get(user_id, User),
    Blogs = blog_blog_mgr:get_user_blogs(UserId),
    lager:info("Blogs for ~p: ~p", [UserId, Blogs]),
    {ok, HTML} = blogs_list:render(SessionData#{blogs=>Blogs}),

    %% HTML = io_lib:format("Request is ~p~nState: ~p~nUser: ~p~nBlogs: ~p", [Req0, State, User, Blogs]),
    Req1 = blog_mid_session:update_session(#{error_message=><<>>}, Req0),

    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			   HTML, Req1),
    {ok, Req, State};


init(Req0 = #{method := <<"POST">>}, State=[add]) ->
    lager:info("Do add blog"),
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData),
    UserId = maps:get(user_id, User),


    {ok, BodyList, Req1} = cowboy_req:read_urlencoded_body(Req0),
    Body = maps:from_list(BodyList),
    BlogName = maps:get(<<"blog_name">>, Body),

    case blog_blog_mgr:add_blog(UserId, BlogName) of
	{ok, _BlogId} ->
	    Resp = cowboy_req:reply(302,
				    #{<<"content-type">> => <<"text/plain">>,
				     <<"location">> => <<"/blogs">>},
				    <<>>,
				    Req1),
	    {ok, Resp, State};

	{error, Reason}->
	    lager:info("ERROR!: ~p", [Reason]),
	    Req2 = blog_mid_session:update_session(#{error_message=>Reason}, Req1),
	    Resp = cowboy_req:reply(302,
				    #{<<"content-type">> => <<"text/plain">>,
				      <<"location">> => <<"/blogs">>},
				    <<>>,
				    Req2),
	    {ok, Resp, State}
    end;

init(Req0 = #{method := <<"GET">>}, State=[action]) ->
    lager:info("Do action on blog, req=~p", [Req0]),
    
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData),
    UserId = maps:get(user_id, User),

    BlogAction = cowboy_req:binding('blog_action', Req0),
    [BlogId, Action] = binary:split(BlogAction, <<":">>),
    lager:info("Do ~p on blog", [Action]),
    Resp = prepare_action(Req0, UserId, binary_to_integer(BlogId), Action),
    {ok, Resp, State};

init(Req0 = #{method := <<"POST">>}, State=[action]) ->
    lager:info("Do action on blog, req=~p", [Req0]),
    
    SessionData = maps:get(session,  Req0),
    User = maps:get(user, SessionData),
    UserId = maps:get(user_id, User),

    BlogAction = cowboy_req:binding('blog_action', Req0),
    [BlogId, Action] = binary:split(BlogAction, <<":">>),
    lager:info("Do ~p on blog", [Action]),
    Resp = perform_action(Req0, UserId, binary_to_integer(BlogId), Action),
    {ok, Resp, State}.




prepare_action(Req0, UserId, BlogId, <<"edit">>)->
    SessionData = maps:get(session,  Req0),

    {ok, Blog}  = blog_blog_mgr:get_blog_by_id(UserId, BlogId),
    {ok, HTML} = blogs_edit:render(SessionData#{blog=>Blog}),

    %% HTML = io_lib:format("<pre>~p</pre>", [Blog]),
    Req1 = blog_mid_session:update_session(#{error_message=><<>>}, Req0),

    cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/html">>},
			   HTML, Req1).

perform_action(Req, UserId, BlogId, <<"edit">>) ->
    {ok, BodyList, Req1} = cowboy_req:read_urlencoded_body(Req),
    Body = maps:from_list(BodyList),
    BlogName = maps:get(<<"blog_name">>, Body),
    BlogTitle = maps:get(<<"blog_title">>, Body),
    BlogSubtitle = maps:get(<<"blog_subtitle">>, Body),
    BlogMap = #{blog_id => BlogId, 
		blog_name => BlogName, 
		user_id => UserId, 
		title => BlogTitle, 
		subtitle =>BlogSubtitle },
    blog_blog_mgr:update_blog(BlogMap),
    cowboy_req:reply(302,
		     #{<<"content-type">> => <<"text/plain">>,
		       <<"location">> => <<"/blogs">>},
		     <<>>,
		     Req1);


perform_action(Req, UserId, BlogId, <<"delete">>) ->
    blog_blog_mgr:delete_blog(UserId, BlogId),
    cowboy_req:reply(302,
		     #{<<"content-type">> => <<"text/plain">>,
		       <<"location">> => <<"/blogs">>},
		     <<>>,
		     Req);

perform_action(Req, _UserId, _BlogId, _Action) ->
    Resp = cowboy_req:reply(302,
				    #{<<"content-type">> => <<"text/plain">>,
				      <<"location">> => <<"/blogs">>},
				    <<>>,
				    Req),
    Resp.


