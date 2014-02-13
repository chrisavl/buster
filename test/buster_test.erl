-module(buster_test).
-include_lib("eunit/include/eunit.hrl").

integration_test() ->
    application:set_env(buster, interval, 1000),

    ?assertEqual(ok, application:start(buster)),

    ?assertEqual({error, memory_threshold_exceeded},
                 (buster_watchers:mem_watcher(0))()),
    ?assertEqual(ok, (buster_watchers:mem_watcher(infinity))()),

    Self = self(),
    buster:set_callback(fun (_, Reason) -> Self ! Reason end),
    buster:add_watcher(buster_watchers:mem_watcher(0)),
    ?assertMatch([_], buster_server:get_watchers()),
    receive
        {error, memory_threshold_exceeded} ->
            ok
    end,

    buster:clear_watchers(),
    ?assertEqual([], buster_server:get_watchers()),

    ok.
