%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(root_handler).
-export([init/2]).


init(Req0, State) ->
	SessionData = maps:get(session,  Req0),
	{ok, HTML} = index_page:render(SessionData),
    Req = cowboy_req:reply(200,
						   #{<<"content-type">> => <<"text/html">>},
						   HTML, Req0),
    {ok, Req, State}.

