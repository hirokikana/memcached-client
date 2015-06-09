-module(memcached_router).

-behaviour(gen_server).

%% -------------------------------------
%%  gen_server callbacks
%% -------------------------------------
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         terminate/2,
         handle_info/2
]).
-export([
         start_link/0,
         add_server/2,
         get_server/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_server(Server, Pid) ->
    gen_server:call(?MODULE, {add, Server, Pid}).

get_server(Key) ->
    gen_server:call(?MODULE, {get, Key}).

init(_Args) ->
    State = [],
    {ok, State}.
handle_call({get, _Key}, _From, State) ->
    Key = 1,
    Value = moyo_assoc:fetch(Key, State),
    {reply, Value, State};
handle_call({add, Server, Pid}, _From, State) ->
    Key = 1,
    NewState = moyo_assoc:store(Key, {Server, Pid}, State),
    {reply, ok, NewState};
handle_call(_Msg, _From, State) ->
    {reply, State}.

handle_cast(_Msg, State) ->
    {reply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



   
    
