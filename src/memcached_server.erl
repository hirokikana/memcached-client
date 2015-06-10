-module(memcached_server).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         code_change/3,
         terminate/2,
         handle_info/2
]).
-export([
         start_link/1,
         stats/1,
         version/1,
         get/2,
         set/3
]).


get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).
set(Pid, Key, Value) ->
    gen_server:call(Pid, {set, Key, Value}).
stats(Pid) ->
    gen_server:call(Pid, {stats}).
version(Pid) ->
    gen_server:call(Pid, {version}).

-spec start_link(memcached:server()) -> ok.
start_link(MemcachedServer) ->
    gen_server:start_link(?MODULE, [MemcachedServer], []).

init(Args) ->
    {Host, Port} = hd(Args),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
    State = {sock, Sock},
    {ok, State}.

handle_call({get, Key}, _From, State) ->
    {sock, Sock} = State,
    gen_tcp:send(Sock, "get " ++ Key ++ "\r\n"),

    {ok, Response} = gen_tcp:recv(Sock,0),
    Result = case Response of 
                 <<"VALUE ", _T/binary>> ->
                     [_, Value|_] = binary:split(Response, <<"\r\n">>, [global]),
                     {ok, binary_to_term(Value)}
             end,
    {reply, Result, State};
handle_call({set, Key, ValueParam}, _From, State) ->
    {sock, Sock} = State, % TODO 再接続しない
    Value = term_to_binary(ValueParam),
    Expire = 100, % TODO
    Size = size(Value),
    RequestLine = ["set " ,Key, " 0 ", integer_to_list(Expire), " " , integer_to_list(Size), "\r\n",Value,"\r\n"],
    gen_tcp:send(Sock, RequestLine),
    Result = case gen_tcp:recv(Sock,0) of 
                 {ok, <<"STORED\r\n">>} ->
                     {ok, stored};
                 Reason ->
                     {error, Reason}
             end,
    {reply, Result, State};
handle_call({stats}, _From, State) ->
    {sock, Sock} = State,
    gen_tcp:send(Sock, "stats\r\n"),
    {ok, Response} = gen_tcp:recv(Sock,0),
    Stats = case Response of
                <<"STAT ", T/binary>> ->
                    lists:map(fun(X) ->
                                      case binary:split(X, <<" ">>, [global]) of
                                          [_, Key, Value] ->
                                              {Key, Value};
                                          _ ->
                                              X
                                      end
                              end,
                              binary:split(T, <<"\r\n">>, [global]))
            end,
    {reply, Stats, State};
handle_call({version}, _From, State) ->
    {sock, Sock} = State,
    gen_tcp:send(Sock, "version\r\n"),
    {ok, Response} = gen_tcp:recv(Sock,0),
    VersionNum = case Response of
                     <<"VERSION ", Version/binary>> ->
                         hd(binary:split(Version,<<"\r\n">>))
                 end,
    {reply, binary_to_list(VersionNum), State};
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



