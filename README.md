Buster
=====

Buster is a general periodic watchdog application for Erlang. You can use it
to set up different periodic monitoring functions and act via a callback when
those monitoring functions indicate something is up.

Note, in most cases you should be able to get away with using Erlang's built
in monitor, e.g. `link`, `monitor` or `os_mon`. Buster is not a replacement
for those.

Watch functions
---------------
The watch functions added to buster with `buster:add_watcher/1` will be called
periodically, if they return anything other than the atom `ok` it is
considered and error and the configured callback is called.

Callbacks
---------
You can either have one global callback which is called for all errors, this
callback is set either as a environment variable called `callback` for the
`buster` application or via `buster:set_callback/1`.

You can also have watcher specific callbacks which override the global one,
to set this up pass in the callback in `buster:add_watcher/2`.

Sample usage
-----

```erlang
application:set_env(buster, interval, 1000).
application:start(buster).
WatchFun = fun () -> case random:uniform() > 0.5 of true -> ok; false -> {error, some_reason} end end.
Callback = fun (_Watcher, Reason) -> io:format("whatcher signaled issue: ~p~n", [Reason]) end.
buster:add_watcher(WatchFun, Callback).
buster:clear_watchers().
```
