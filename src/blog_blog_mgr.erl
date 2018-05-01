%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_blog_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).


-export([get_user_blogs/1, get_blog_by_id/2,get_blog_by_name/2,add_blog/2, update_blog/1, delete_blog/2]).
-define(SERVER, ?MODULE).

-record(state, {initial_ts::integer()}).
-record(blog,{blog_id, blog_name, user_id, title, subtitle}).

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


-spec get_user_blogs(UserId::integer())->[map()].
get_user_blogs(UserId)->
    gen_server:call(?SERVER, {get_user_blogs, UserId}).

-spec get_blog_by_id(UserId::integer(), BlogId::integer())-> {ok, map()}|{error, term()}.
get_blog_by_id(UserId, BlogId)->
    gen_server:call(?SERVER, {get_blog_by_id, UserId, BlogId}).

-spec get_blog_by_name(UserId::integer(), BlogName::binary())-> {ok, map()}|{error, term()}.
get_blog_by_name(UserId, BlogName)->
    gen_server:call(?SERVER, {get_blog_by_name, UserId, BlogName}).

-spec add_blog(UserId::integer(), BlogName::binary())->{ok, BlogId::integer() }|{error, term()}.
add_blog(UserId, BlogName)->
    gen_server:call(?SERVER, {add_blog, UserId, BlogName}).

-spec update_blog(Blog::map())->ok|{error, term()}.
update_blog(Blog)->
    gen_server:call(?SERVER, {update_blog, Blog}).

-spec delete_blog(UserId::integer(), BlogId::integer())-> ok|{error, term()}.
delete_blog(UserId, BlogId)->
    gen_server:call(?SERVER, {delete_blog, UserId, BlogId}).


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
				  blog, [
						 {record_name, blog},
						 {attributes, record_info(fields, blog)},
						 {index, [#blog.blog_name, #blog.user_id]},
						 {disc_copies, [node()]}
						])),
	ok = mnesia:wait_for_tables([blog], infinity),
	{ok, #state{initial_ts = os:system_time(nanosecond)}}.


handle_call({get_user_blogs, UserId}, _From, State)->
    BlogsRecords =  mnesia:dirty_index_read(blog, UserId, #blog.user_id),
    BlogsMaps = lists:map(fun blog_rec_to_map/1,BlogsRecords),
    {reply, BlogsMaps, State};

handle_call({update_blog, BlogMap}, _From, State)->
    BlogRec = blog_map_to_rec(BlogMap),
    Reply = mnesia:activity(
	      transaction, 
	      fun()-> mnesia:write(BlogRec) end ),
    {reply, Reply, State};


handle_call({add_blog, UserId, BlogName}, _From, State = #state{initial_ts = InitialTs})->
    lager:info("Add blog for ~p: ~p", [UserId, BlogName]),
    Reply = try 
		mnesia:activity(
	      transaction, 
	      fun()->
		      case mnesia:match_object({blog, '_', BlogName, UserId, '_', '_'})  of
			  [] ->
			      BlogId = InitialTs+erlang:unique_integer([positive,monotonic]),
			      BlogRec = #blog{
					   blog_id = BlogId, 
					   blog_name = BlogName, 
					   user_id = UserId, 
					   title = <<"Untitled">>, 
					   subtitle = <<"Subtitle">>},
			      ok = mnesia:write(BlogRec),
			      lager:info("Added blog record OK"),
			      {ok, {ok, BlogId}};
			  Found ->
			      lager:info("Found records : ~p", [Found]),
			      mnesia:abort( blog_exists)
		      end 
	      end)
	    catch 
		CLASS:ERROR ->
		lager:info("delete Failed as ~p:~p", [CLASS, ERROR]),
		{error, blog_exists}
	    end,
    {reply, Reply, State};

handle_call({get_blog_by_name, UserId, BlogName}, _From, State)->
    UseUserId = case UserId of
		    undefined -> '_';
		    _ -> UserId
		end,
    Reply =
	case mnesia:dirty_match_object({blog, '_', BlogName, UseUserId, '_', '_'})  of
	    [Blog] ->
		{ok, blog_rec_to_map(Blog)};
	    _ ->
		{error, not_found}
	end,
    {reply, Reply, State};

handle_call({get_blog_by_id, UserId, BlogId}, _From, State)->
    Reply = 
	case mnesia:dirty_match_object({blog, BlogId, '_', UserId, '_', '_'})  of
	    [Blog] ->
		{ok, blog_rec_to_map(Blog)};
	    _ ->
		{error, not_found}
	end,
    {reply, Reply, State};

handle_call({delete_blog, UserId, BlogId}, _From, State)->
    Reply = 
	try mnesia:activity(
	      transaction, 
	      fun()->
		      case mnesia:match_object({blog, BlogId, '_', UserId, '_', '_'})  of
			  [Blog] ->
			      mnesia:delete_object(Blog);
			  _ ->
			      mnesia:abort(not_found)
		      end
	      end)
	catch 
	    CLASS:ERROR -> 
		lager:info("delete Failed as ~p:~p", [CLASS, ERROR]),
		{error, not_found}
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



%% INTERNAL
blog_rec_to_map(#blog{blog_id=BlogId, blog_name=BlogName, user_id=UserId, title=Title, subtitle=SubTitle})->
    #{blog_id=>BlogId, blog_name=>BlogName, user_id=>UserId, title=>Title, subtitle=>SubTitle}.



blog_map_to_rec(#{blog_id:=BlogId, blog_name:=BlogName, 
		  user_id:=UserId, title:=Title, subtitle:=SubTitle}) ->
    #blog{blog_id=BlogId, blog_name=BlogName, user_id=UserId, title=Title, subtitle=SubTitle}.
