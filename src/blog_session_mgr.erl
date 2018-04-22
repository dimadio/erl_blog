%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 12 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_session_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([add_session/2, get_session/1, save_session/2, del_session/1]).



-define(SERVER, ?MODULE).

-record(state, {}).
-record(session,{sess_id, sess_data}).





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

add_session(SessId, Data)->
	gen_server:call(?MODULE, {add_session, SessId, Data}).

get_session(SessId)->
	gen_server:call(?MODULE, {get_session, SessId}).

save_session(SessId, Data)->
	gen_server:call(?MODULE, {save_session, SessId, Data}).

del_session(SessId)->
	gen_server:call(?MODULE, {del_session, SessId}).

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
				  session, [
							{record_name, session},
							{attributes, record_info(fields, session)},
							{disc_copies, [node()]}
						   ])),
	ok = mnesia:wait_for_tables([session], infinity),
	{ok, #state{}}.

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
handle_call({add_session, SessId, Data}, _From, State)->
	Res = 
		mnesia:activity(
		  transaction, 
		  fun()->
				  case mnesia:dirty_read(session, SessId) of
					  [] ->
						  mnesia:write(#session{sess_id = SessId, 
												sess_data = Data});
					  _ ->
						  mnesia:abort(id_collision)
				  end 
		  end),
	{reply, Res, State};

handle_call({save_session, SessId, Data}, _From, State)->
	Res = 
		mnesia:activity(
		  transaction, 
		  fun()->
				  case mnesia:dirty_read(session, SessId) of
					  [#session{sess_id = SessId}] ->
						  mnesia:write(#session{sess_id = SessId, 
												sess_data = Data});
					  [] ->
						  mnesia:abort(id_collision)
				  end 
		  end),
	{reply, Res, State};

handle_call({get_session, SessId}, _From, State)->
	Res = case mnesia:dirty_read(session, SessId) of
			  [] ->
				  undefined;
			  [#session{sess_id = SessId, 
						sess_data = Data}] ->
				  Data
		  end,
	{reply, Res, State};

handle_call({del_session, SessId}, _From, State)->
	Res = 
		mnesia:activity(
		  transaction, 
		  fun()->
				  mnesia:dirty_delete(session, SessId)
		  end),
	{reply, Res, State};
			
handle_call(_Request, _From, State) ->
	Reply = ok,
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



