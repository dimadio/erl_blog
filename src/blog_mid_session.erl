%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_mid_session).

-behavior(cowboy_middleware).

-export([execute/2]).

-export([get_session_id/1, update_session/2, delete_session/1]).


get_session_id(Req)->
	Cookies = cowboy_req:parse_cookies(Req),
	proplists:get_value(<<"blog_session">>, Cookies).

update_session(Delta, Req)->
	SessionData = maps:get(session,  Req),
	UpdatedData = maps:merge(SessionData, Delta),
	SessId = get_session_id(Req),
	blog_session_mgr:save_session(SessId, UpdatedData),
	maps:put(session, UpdatedData, Req).

delete_session(Req)->
	SessId = get_session_id(Req),
	blog_session_mgr:del_session(SessId),
	Req2 = maps:put(session, #{}, Req),
	cowboy_req:set_resp_cookie(<<"blog_session">>, <<>>, Req2).

execute(Req, Env) ->	
	SessionID = get_session_id(Req),
	case SessionID of 
		undefined ->
			NewSessionID = create_id(32),
			blog_session_mgr:add_session(NewSessionID, #{}),
			Req2 = maps:put(session, #{}, Req),
			Req3 = cowboy_req:set_resp_cookie(<<"blog_session">>, NewSessionID, Req2),
			{ok, Req3, Env};
		_ ->
			Data = blog_session_mgr:get_session(SessionID),
			Req2 = case Data of
					   undefined ->
						   blog_session_mgr:add_session(SessionID, #{}),
						   maps:put(session, #{}, Req);
					   _ -> 
						   maps:put(session, Data, Req)
				   end,
			{ok, Req2, Env}
	end.


create_id(N)->
	AllowedCharacters = <<"ABCDEFGHIJKLMNPQRSTUVWXYZ123456789!^~">>,
	RandomSet = binary_to_list(crypto:strong_rand_bytes(N)),
	Size = size(AllowedCharacters),
	list_to_binary(lists:map(
					 fun(B) -> 
							 binary:at(AllowedCharacters, B rem Size) end, 
					 RandomSet)).
