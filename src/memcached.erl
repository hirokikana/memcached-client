%% ===================================================================
%%  memcached
%% ===================================================================
-module(memcached).

%% ===================================================================
%% Exported API
%% ===================================================================
-export([
         start_link/1,
         stats/0,
         get/1,
         set/2,
         set/3,
         delete/1,
         version/0
]).

%% ===================================================================
%% Types
%% ===================================================================
-type key() :: term().
-type value() :: term().
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

%% ===================================================================
%% Exported Function
%% ===================================================================
%% @doc 指定したmemcachedサーバーを利用するように起動を行う
-spec start_link(server_list()) -> ok.
start_link(ServerList) ->
    Server = hd(ServerList),
    {ok, _} = memcached_sup:start_router(),

    {ok, Pid} = memcached_sup:start_server(Server),
    memcached_router:add_server(Server, Pid).

%% @doc memcachedサーバーの統計情報を取得する
%%  TODO どのmemcachedサーバーの情報を取得するか指定できるようにする
-spec stats() -> [tuple()].
stats() ->
    %% memcached_routerから適切なPidを取得してくる
    Key = aa,
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:stats(Pid).

%% @doc memcachedサーバーのバージョンを取得する
%%  TODO どのmemcachedサーバーの情報を取得するか指定できるようにする
-spec version() -> string().
version() ->
    Key = aa,
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:version(Pid).

%% @doc 指定したキーと値をmemcachedにセットする
%%  Expireは0(無期限)で指定される
%%  @see set/3
%%  TODO エラー時の戻り値
-spec set(key(), value()) -> {ok, term()}.
set(Key, Value) ->
    set(Key, Value, 0).

%% @doc 指定したキーと値を指定したexpireでmemcachedにセットする
%%  TODO エラー時の戻り値
-spec set(key(), value(), integer()) -> {ok, term()}.
set(Key, Value, Expire) ->
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:set(Pid, Key, Value, Expire).

%% @doc 指定したキーの値を取得する
%%  TODO エラー時の戻り値
-spec get(key()) -> {ok, term()}.
get(Key) ->
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:get(Pid, Key).

%% @doc 指定したキーを削除する
-spec delete(key()) -> {ok, term()}.
delete(Key) ->
    {_, Pid} = memcached_router:get_server(Key),
    memcached_server:delete(Pid, Key).
