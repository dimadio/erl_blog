-module(blog_tools).
-export([init_db/0, 
		 is_ok/1]).


init_db()->
	ok = is_ok(mnesia:create_schema([node()])),
	ok = mnesia:start().


is_ok({atomic, ok}) -> ok;
is_ok({aborted, {already_exists, _}}) -> ok;
is_ok({error,{N,{already_exists,N}}}) -> ok;
is_ok(Err) -> Err.
	
	
