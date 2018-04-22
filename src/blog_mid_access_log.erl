%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_mid_access_log).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->	
	Method = cowboy_req:method(Req),
	Path = cowboy_req:path(Req),

	{Peer, _} = cowboy_req:peer(Req),
	IpAddr = case inet_parse:ntoa(Peer) of 
				 List when is_list(List) -> List;
				 _                 -> "-"
			 end,
	lager:info("~s ~s ~s", [IpAddr, Method, Path]),
	Req2 = cowboy_req:set_resp_header(<<"server">>, <<"blog engine 0.1">>, Req),
	{ok, Req2, Env}.

