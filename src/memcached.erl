-module(memcached).

-export([
         start_link/1,
         stats/0,
         get/1,
         set/2,
         set/3,
         delete/1,
         version/0
]).


-type key() :: term().
-type server_host() :: atom().
-type server_port() :: atom().
-type server() :: {server_host(), server_port()}.
-type server_list() :: [server()].
-export_type(
   [
    server/0,
    key/0
   ]
).

-spec start_link(server_list()) -> ok.
start_link(ServerList) ->
    Server = hd(ServerList),
    {ok, RouterPid} = memcached_sup:start_router(),

    {ok, Pid} = memcached_sup:start_server(Server),
    memcached_router:add_server(Server, Pid).

stats() ->
    %% memcached_routerから適切なPidを取得してくる
    Key = aa,
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:stats(Pid).

version() ->
    Key = aa,
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:version(Pid).

set(Key, Value) ->
    set(Key, Value, 0).

set(Key, Value, Expire) ->
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:set(Pid, Key, Value, Expire).

get(Key) ->
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:get(Pid, Key).

delete(Key) ->
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:delete(Pid, Key).
