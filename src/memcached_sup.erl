-module(memcached_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_server/1,
         start_router/0
]).

%% Supervisor callbacks
-export([
         init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_server(memcached:server()) -> supervisor:startchild_ret().
start_server(MemcachedServer) ->
    {Host, Port} = MemcachedServer,
    ChildSpec = {MemcachedServer,
                 {memcached_server, start_link, [MemcachedServer]},
                 permanent,
                 5000,
                 worker,
                 [memcached_server]},
    supervisor:start_child(?MODULE, ChildSpec).

start_router() ->
    ChildSpec = {{memcached_router,local},
                 {memcached_router, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [memcached_router]},
    supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

