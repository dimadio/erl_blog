%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2018 by  <>
%%%-------------------------------------------------------------------
%% {ok,1524045758348750241,<<"AXZBITU5">>}
-module(blog_user_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([create_user/1,
		 login_user/2,
		 change_user_password/3,
		 get_user/1,
		 set_user_data/2]).



-define(SERVER, ?MODULE).

-record(state, {initial_ts::integer()}).
-record(user,{user_id, login, password_hash, password_salt, user_data}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_user(Login::binary())-> {ok, UserID, Password}|{error, Reason}
										 when UserID::binary(),
											  Password::binary,
											  Reason::term().
create_user(Login)->
	gen_server:call(?SERVER, {create_user,Login}).



-spec login_user(Login, Password)->{ok, User}|{error, Reason} 
									   when Login::binary(),
											Password::binary(),
											User::map(),
											Reason::term().
login_user(Login, Password)->
	gen_server:call(?SERVER, {login_user, Login, Password}).



-spec change_user_password(Login, OldPass, NewPass)-> 
				  ok|{error, Reason} 
				      when Login::binary(),
					   OldPass::binary(),
					   NewPass::binary(),
					   Reason::term().
change_user_password(Login, OldPass, NewPass)->
	gen_server:call(?SERVER, {change_user_password,Login, OldPass, NewPass}).



-spec get_user(UserID)-> {ok, User}|{error, Reason}
							 when UserID::integer(),
								  User::map(),
								  Reason::term().
get_user(UserID)->
	gen_server:call(?SERVER, {get_user, UserID}).


-spec set_user_data(UserID, UserData)-> 
						   ok|{error,Reason}
							   when UserID::integer(),
									UserData::map(),
									Reason::term().
set_user_data(UserID, UserData)->
	gen_server:call(?SERVER, {set_user_data, UserID, UserData}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	lager:info("Mnesia config: ~p", [mnesia:info()]),

	ok = blog_tools:is_ok(mnesia:create_table(
				  user, [
						 {record_name, user},
						 {attributes, record_info(fields, user)},
						 {index, [#user.login]},
						 {disc_copies, [node()]}
						])),
	ok = mnesia:wait_for_tables([user], infinity),
	{ok, #state{initial_ts = os:system_time(nanosecond)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_user,Login}, _From, State = #state{initial_ts = InitialTs}) ->
	Reply = mnesia:activity(
			  transaction, 
			  fun()->
					  case mnesia:dirty_index_read(user, Login, #user.login) of
						  [] ->
							  UserID = InitialTs+erlang:unique_integer([positive,monotonic]),
							  PasswordSalt = crypto:strong_rand_bytes(32),
							  Password = build_rand_password(8),
							  PasswordHash = hash(Password, PasswordSalt),
							  UserRec = #user{
										   user_id = UserID, 
										   login = Login, 
										   password_hash=PasswordHash, 
										   password_salt=PasswordSalt, 
										   user_data=#{} },
							  ok = mnesia:write(UserRec),
							  {ok, UserID, Password};
					  _ ->
						  mnesia:abort(id_collision)
				  end 
		  end),
	{reply, Reply, State};


handle_call({login_user, Login, Password}, _From, State)->
	Reply = 
	case mnesia:dirty_index_read(user, Login, #user.login) of
	    []->
		{error, not_found};
	    [#user{user_id=UserID,
		   login=Login, 
		   password_hash=PasswordHash, 
		   password_salt=PasswordSalt,
		   user_data = UserData}]->

		case hash(Password, PasswordSalt) of
		    PasswordHash ->
			{ok, maps:merge(#{login => Login, user_id => UserID},UserData) };
		    _ ->
			{error, wrong_password}
		end
	end,
    {reply, Reply, State};
	
handle_call(_Request, _From, State) ->
	Reply = {error, not_implemented},
	{reply, Reply, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_rand_password(Size)->
	Bytes = <<"ABCDEFGHIJKLMNPQRSTUVWXYZ123456789">>,
	Count = size(Bytes),
	Bin = crypto:strong_rand_bytes(Size),
	<< <<(binary:at(Bytes, N rem Count)):8>> || <<N:8>> <= Bin >>.


hash(Password, PasswordSalt)->
	crypto:hmac(sha256, PasswordSalt, << Password/binary, PasswordSalt/binary>> ).
