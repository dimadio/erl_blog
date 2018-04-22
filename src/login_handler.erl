%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(login_handler).

-export([init/2]).


init(Req0 = #{method := <<"GET">>}, State) ->
	SessionData = maps:get(session,  Req0),
	{ok, HTML} = login_page:render(SessionData),
	Req1 = blog_mid_session:update_session(#{error_message=><<>>}, Req0),

    Req = cowboy_req:reply(200,
						   #{<<"content-type">> => <<"text/html">>},
						   HTML, Req1),
    {ok, Req, State};


init(Req0 = #{method := <<"POST">>}, State) ->
	{ok, BodyList, Req1} = cowboy_req:read_urlencoded_body(Req0),
	Body = maps:from_list(BodyList),
	lager:info("Body=~p", [Body]),

	case validate_login(Body) of
		ok ->
			{ok, User} = get_user(Body),
			Req2 = blog_mid_session:update_session(#{user=>User}, Req1),
			Resp = cowboy_req:reply(302,
								   #{<<"content-type">> => <<"text/plain">>,
									 <<"location">> => <<"/">>},
								   <<>>,
								   Req2),
			{ok, Resp, State};
		Err ->
			Req2 = blog_mid_session:update_session(#{error_message=>Err}, Req1),
			Resp = cowboy_req:reply(302,
								   #{<<"content-type">> => <<"text/plain">>,
									 <<"location">> => <<"/login">>},
								   <<>>,
								   Req2),
			{ok, Resp, State}
end.


validate_login(Body)->
	case Body of
		#{<<"email">> := Login,
		  <<"password">> := Password} when 
			  Login =/= <<>> andalso Password =/= <<>> ->
			ok;
		_ ->
			<<"Please provide login credentials">>
	end.

get_user(Body)->
	#{<<"email">> := Login,
	  <<"password">> := Password} = Body,
	blog_user_mgr:login_user(Login, Password).
