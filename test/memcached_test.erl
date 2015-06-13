%% coding: latin-1

-module(memcached_test).
-include_lib("eunit/include/eunit.hrl").

memcached_test_() ->
    {
      foreach,
      fun() -> 
              application:ensure_started(memcached)
      end,
      fun(_) -> 
              application:stop(memcached)
      end,
      [
       {"SET/GET",
        fun() ->
                ServerList = [{"localhost", 11211}],
                ?assertEqual(ok,memcached:start_link(ServerList)),
                ?assertEqual({ok, stored},memcached:set("hoge", "fuga")),
                ?assertEqual({ok, "fuga"},memcached:get("hoge")),
                ?assertEqual({ok, stored},memcached:set("hoge", 123)),
                ?assertEqual({ok, 123},memcached:get("hoge")),
                ?assertEqual({ok, stored},memcached:set("hoge", <<"binary">>)),
                ?assertEqual({ok, <<"binary">>},memcached:get("hoge"))
        end},
       {"DELETE",
        fun() ->
                ServerList = [{"localhost", 11211}],
                ?assertEqual(ok, memcached:start_link(ServerList)),
                ?assertEqual({ok, stored}, memcached:set("hoge", "fuga")),
                ?assertEqual({ok, "fuga"}, memcached:get("hoge")),
                ?assertEqual({ok, deleted}, memcached:delete("hoge")),
                ?assertEqual({ok, not_exist}, memcached:get("hoge"))
        end},
       {"Expire",
        fun() ->
                ServerList = [{"localhost", 11211}],
                ?assertEqual(ok, memcached:start_link(ServerList)),
                ?assertEqual({ok, stored}, memcached:set("hoge", "fuga", 1)),
                ?assertEqual({ok, "fuga"}, memcached:get("hoge")),
                timer:sleep(1000),
                ?assertEqual({ok, not_exist}, memcached:get("hoge"))
        end}
       ]}.
               
               
              
