%%%-------------------------------------------------------------------
%%% @author  <dmitry@shestak.me>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 05 May 2018 by  <>
%%%-------------------------------------------------------------------
-module(blog_entry_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([add_entry/4, get_entry/2, get_entries/2]).

-define(SERVER, ?MODULE).

-record(state, {initial_ts::integer()}).
-record(entry,{entry_id, blog_id, title, blob_path, tags, timestamp}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add_entry(BlogId::integer(), Title::binary(), Content::binary(), Tags::[binary()]) 
	       -> {ok, integer()}|{error, term()}.
add_entry(BlogId, Title, Content, Tags)->
    gen_server:call(?SERVER, {add_entry, BlogId, Title, Content, Tags}).

-spec get_entry(BlogId::integer(), Title::binary())->{ok, map()}|{error, term()}.
get_entry(BlogId, Title)->
    gen_server:call(?SERVER, {get_entry, BlogId, Title}).

-spec get_entries(BlogId::integer(), Opts::[tuple()])->{ok, [map()]}|{error, term()}.
get_entries(BlogId, Opts)->
    gen_server:call(?SERVER, {get_entries, BlogId, Opts}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
				  entry, [
						 {record_name, entry},
						 {attributes, record_info(fields, entry)},
						 {index, [#entry.blog_id, #entry.title]},
						 {disc_copies, [node()]}
						])),
	ok = mnesia:wait_for_tables([entry], infinity),
        mnesia:transform_table(entry, fun(R)->
					    lager:info("Record is ~p", [R]),
					    R end, record_info(fields, entry)),

	{ok, #state{initial_ts = os:system_time(nanosecond)}}.



handle_call({add_entry, BlogId, Title, Content, Tags}, _From, State = #state{initial_ts = InitialTs}) ->
    EntryId = InitialTs+erlang:unique_integer([positive,monotonic]),
    EntryPath=build_entry_path(BlogId, EntryId),
    ok = save_entry(EntryPath, Content),
    Reply = try 
		mnesia:activity(
		  transaction, 
		  fun()->
			  EntryRec = #entry{entry_id = EntryId, 
					    blog_id = BlogId, 
					    title = Title, 
					    blob_path = EntryPath, 
					    tags = Tags,
					    timestamp = os:system_time()},
			      ok = mnesia:write(EntryRec),
			  lager:info("Added blog entry OK"),
			  {ok, {ok, EntryId}}
		     end)
	    catch
		CLASS:ERROR ->
		    delete_entry(EntryPath),
		    {error, {add_failed, CLASS, ERROR}}
	    end,
    {reply, Reply, State};

handle_call({get_entries, BlogId, _Opts}, _From, State) ->
    Entries =  mnesia:dirty_index_read(entry, BlogId, #entry.blog_id),
    SortedEntries = lists:sort(fun(#entry{timestamp = Ats},#entry{timestamp = Bts})->
				       Bts < Ats
			       end, Entries),
    lager:info("SortedEntries: ~p", [SortedEntries]),
    EntriesMaps = lists:map(fun entry_rec_to_map/1,SortedEntries),
    {reply, {ok, EntriesMaps}, State};


handle_call({get_entry, BlogId, Title}, From , State)->
    Reply =case mnesia:dirty_match_object({entry, '_', BlogId, Title, '_', '_', '_'})  of
	       [] -> {error, not_found};
	       [EntryRec] ->
		   {ok, entry_rec_to_map(EntryRec)}
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



build_entry_path(BlogId, EntryId)->
    BlogIdBin = integer_to_binary(BlogId),
    EntryIdBin = integer_to_binary(EntryId),
    FileNameBase = << "B", BlogIdBin/binary, "E", EntryIdBin/binary>>,
    FileHash = crypto:hmac(sha512, EntryIdBin, FileNameBase),
    FileHashBin = << << (integer_to_binary(N, 16))/binary >> || <<N:8>> <= FileHash >>,

    {ok, SpoolFolder} = application:get_env(blog_simple, spool_folder),

    << F:2/binary, S:2/binary, _/binary >> = FileHashBin,
    filename:join([SpoolFolder, F,S, FileHashBin]).


save_entry(EntryPath, Content)->
    ok = filelib:ensure_dir(EntryPath),
    file:write_file(EntryPath, Content).

load_entry(EntryPath)->
    {ok, Content} = file:read_file(EntryPath),
    Content.

delete_entry(EntryPath)->
    file:delete(EntryPath).


entry_rec_to_map(#entry{entry_id=EntryId, blog_id=BlogId, title=Title, blob_path=EntryPath, tags=Tags, timestamp=Timestamp})->    
    Content = load_entry(EntryPath),
    #{entry_id=>EntryId, blog_id=>BlogId, title=>Title, content=> Content, 
      html_content => markdown:conv(binary:bin_to_list(Content)),
      tags=>Tags, timestamp=>Timestamp}.
