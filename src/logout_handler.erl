-module(logout_handler).

-export([init/2]).

init(Req0, State) ->
	Req1 = blog_mid_session:delete_session(Req0),
	Resp = cowboy_req:reply(302,
							#{<<"content-type">> => <<"text/plain">>,
							  <<"location">> => <<"/">>},
							<<>>,
							Req1),
	{ok, Resp, State}.
	
