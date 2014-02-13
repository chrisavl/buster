-module(buster_server).

-behaviour(gen_server).

-define(DEFAULT_CHECK_INTERVAL, 60000). %% 1 minute

%% API
-export([start_link/0,
         start_link/1,
         add_watcher/2,
         add_watcher/1,
         clear_watchers/0,
         set_callback/1]).

-export([get_watchers/0]). %% exported for testing

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {interval, watchers, callback}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() -> start_link([]).

start_link(Args) ->
    Interval = proplists:get_value(interval, Args, ?DEFAULT_CHECK_INTERVAL),
    Callback = proplists:get_value(callback, Args, fun default_global_callback/2),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Interval, Callback], []).

add_watcher(WatchFun, CallbackFun) ->
    gen_server:call(?MODULE, {add_watcher, {WatchFun, CallbackFun}}).

add_watcher(WatchFun) ->
    add_watcher(WatchFun, undefined).

clear_watchers() ->
    gen_server:call(?MODULE, clear_watchers).

set_callback(Callback) ->
    gen_server:call(?MODULE, {set_callback, Callback}).

get_watchers() ->
    gen_server:call(?MODULE, get_watchers).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CheckInterval, Callback]) ->
    erlang:send_after(CheckInterval, self(), check),
    {ok, #state{interval = CheckInterval,
                callback = Callback,
                watchers = ordsets:new()}}.


handle_call({add_watcher, Watcher}, _From, #state{watchers = Watchers} = State) ->
    {reply, ok, State#state{watchers = ordsets:add_element(Watcher, Watchers)}};

handle_call(clear_watchers, _From, State) ->
    {reply, ok, State#state{watchers = ordsets:new()}};

handle_call({set_callback, Callback}, _From, State) ->
    {reply, ok, State#state{callback = Callback}};

handle_call(get_watchers, _From, State) ->
    {reply, State#state.watchers, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(check, #state{watchers = Watchers, callback = GlobalCallback} = State) ->
    erlang:send_after(State#state.interval, self(), check),
    lists:foreach(fun ({WatchFun, Callback}) ->
                          case catch WatchFun() of
                              ok ->
                                  ok;
                              WatchResult when Callback =:= undefined ->
                                  call(GlobalCallback, [WatchFun, WatchResult]);
                              WatchResult ->
                                  call(Callback, [WatchFun, WatchResult])
                          end
                  end, ordsets:to_list(Watchers)),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

default_global_callback(_Watcher, _WatchResult) ->
    ok.

call({Mod, Fun}, Args) ->
    erlang:apply(Mod, Fun, Args);

call(Fun, Args) ->
    erlang:apply(Fun, Args).
