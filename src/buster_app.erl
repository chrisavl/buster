-module(buster_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() -> application:start(buster).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    buster_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
