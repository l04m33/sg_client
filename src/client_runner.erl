-module(client_runner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    ServerIP = client:get_opt(server_ip),
    ServerPort = client:get_opt(server_port),
    TestCase = client:get_opt(test_case),
    SpawnInterval = client:get_opt(spawn_interval),
    ClientNum = client:get_opt(client_num),

    scene:init_masks(),
    F = fun(_) ->
        client_spawner:spawn_client(ServerIP, ServerPort, TestCase),
        timer:sleep(SpawnInterval)
    end,
    lists:foreach(F, lists:seq(1, ClientNum)),
    {ok, no_use}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

