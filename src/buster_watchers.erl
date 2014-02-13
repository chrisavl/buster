-module(buster_watchers).

-export([mem_watcher/1,
         node_watcher/0,
         node_watcher/1]).

mem_watcher(Threshold) ->
    fun () ->
            case erlang:memory(total) > Threshold of
                true ->
                    {error, memory_threshold_exceeded};
                false ->
                    ok
            end
    end.

node_watcher() ->
    Nodes = [node() | nodes()],
    node_watcher(Nodes).

node_watcher(Nodes) when is_list(Nodes) ->
    fun () ->
            lists:foreach(fun (Node) ->
                                  case net_adm:ping(Node) of
                                      pong -> ok;
                                      pang -> {error, {nodedown, Node}}
                                  end
                          end, Nodes)
    end.
