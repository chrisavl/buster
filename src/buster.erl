-module(buster).


-export([add_watcher/2,
         add_watcher/1,
         clear_watchers/0,
         set_callback/1]).

add_watcher(WatchFun) ->
    buster_server:add_watcher(WatchFun).

add_watcher(WatchFun, CallbackFun) ->
    buster_server:add_watcher(WatchFun, CallbackFun).

clear_watchers() ->
    buster_server:clear_watchers().

set_callback(Callback) ->
    buster_server:set_callback(Callback).
