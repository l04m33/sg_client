-module(client_spawner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, spawn_client/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    id_seq = 0}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

spawn_client(ServerIP, ServerPort, Case) ->
    ?SERVER ! {spawn_client, ServerIP, ServerPort, Case},
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({spawn_client, ServerIP, ServerPort, Case}, State) ->
    ID = State#state.id_seq + 1,
    ?I("Spawning client, ID = ~w", [ID]),
    supervisor:start_child(client_sup, 
                           {ID, {client, start_link, [ID, ServerIP, ServerPort, Case]}, 
                            temporary, 5000, worker, [client]}),
    {noreply, State#state{id_seq = ID}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

