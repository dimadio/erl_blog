%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_mid_login).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	%% lager:info("Req: ~p", [Req]),
	%% lager:info("Env: ~p", [Env]),
	PublicPages = maps:get(public_pages, Env, []),
	Path = cowboy_req:path(Req),	
	lager:info("Path: ~p", [Path]),
	IsPublic = case Path of 
				   << "/s/", _/binary >> ->
					   true;
				   _ -> lists:member(Path, PublicPages)
			   end,
	%% lager:info("IsPublic ~p", [IsPublic]),
	Session = maps:get(session, Req, #{}),
	lager:info("Session in Req is ~p", [Session]),
	User = case maps:get(user, Session, undefined) of
			   undefined ->
				   %% lager:info("No user"),
				   undefined;
		U -> 
				   %% lager:info("Have user ~p", [U]),
				   U
				   
		   end,
	if
		User == undefined andalso not IsPublic ->
			%% lager:info("Go home"),
			Resp = cowboy_req:reply(302,
								   #{<<"content-type">> => <<"text/plain">>,
									 <<"location">> => <<"/">>},
								   <<>>,
								   Req),
			{stop, Resp};
		true ->
		{ok, Req, Env}
	end.
		   
	
