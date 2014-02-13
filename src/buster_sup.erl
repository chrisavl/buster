-module(buster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [buster_server()]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

buster_server() ->
    AppOptions = [interval, callback],
    Args = lists:foldl(fun (Option, Acc) ->
                               case application:get_env(buster, Option) of
                                   {ok, Value} ->
                                       [{Option, Value} | Acc];
                                   undefined ->
                                       Acc
                               end
                       end, [], AppOptions),
    {buster_server, {buster_server, start_link, [Args]},
     permanent, 5000, worker, []}.
