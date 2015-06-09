%% coding: latin-1

-module(memcached_test).
-include_lib("eunit/include/eunit.hrl").

memcached_test_() ->
    {
      setup,
      fun() -> application:ensure_started(memcached) end,
      fun(Started) -> ok end,
      [
       {"stats",
        fun() ->
                ServerList = [{localhost, 11211}],
                ?assertEqual(ok,memcached:start_link(ServerList)),
                ?assertEqual({ok, stored},memcached:set("hoge", "fuga")),
                ?assertEqual({ok, "fuga"},memcached:get("hoge")),
                ?assertEqual({ok, stored},memcached:set("hoge", 123)),
                ?assertEqual({ok, 123},memcached:get("hoge")),
                ?assertEqual({ok, stored},memcached:set("hoge", <<"binary">>)),
                ?assertEqual({ok, <<"binary">>},memcached:get("hoge"))
        end}
       ]}.
               
               
              
