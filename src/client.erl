-module(client).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("log.hrl").
-include("client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-record(state, {
    client_id = 0,
    server = {"", 0},       % {ServerIP, ServerPort}
    socket = undefined,
    player_account = "",
    player_id = 0,
    role_id = 0,
    scene_id = 0,
    x = 0,
    y = 0}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ID, ServerIP, ServerPort) ->
    gen_fsm:start_link(?MODULE, [ID, ServerIP, ServerPort], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([ID, ServerIP, ServerPort]) ->
    erlang:process_flag(trap_exit, true),
    NState = #state{
        client_id = ID, 
        server = {ServerIP, ServerPort},
        player_account = ?CLIENT_ACCOUNT_PREFIX ++ integer_to_list(ID)
    },

    ?I("Client started, ServerIP = ~s, ServerPort = ~w", [ServerIP, ServerPort]),
    self() ! connect,
    {ok, idle, NState}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.


handle_info(connect, idle, State) ->
    ?I("Connecting...."),
    {IP, Port} = State#state.server,
	case gen_tcp:connect(IP, Port, [binary, {packet, 2}, {active, true}]) of
        {ok, Socket} ->
            sender:init_sender(Socket),
            NState = State#state{socket = Socket},
            self() ! login,
            {next_state, connected, NState};
        {error, Reason} ->
            ?E("Client ~w failed to connect, IP = ~s, Port = ~w, Reason = ~w",
               [State#state.client_id, IP, Port, Reason]),
            self() ! {stop, Reason},
            {next_state, disconnected, State}
    end;

handle_info(login, connected, State) ->
    Account = State#state.player_account,
    Packet = pt:write(10000, Account),
    sender:send(Packet),
    {next_state, wait_for_login_reply, State};

handle_info({tcp, _Socket, InPacket}, wait_for_login_reply, State) ->
    {ok, {Stat, PlayerID}} = pt:read(10000, InPacket),
    case Stat of
        2 ->        % login success
            Packet = pt:write(10003, 0),
            sender:send(Packet),
            {next_state, wait_for_enter_reply, State#state{player_id = PlayerID}};
        1 ->        % create new role
            NRoleID = rand_server:rand(1, 6),
            Packet = pt:write(10002, {NRoleID, State#state.player_account}),
            sender:send(Packet),
            {next_state, wait_for_create_role_reply, State};
        Other ->
            exit({unknown_stat_from_server, Other})
    end;

handle_info({tcp, _Socket, InPacket}, wait_for_create_role_reply, State) ->
    {ok, {Stat, PlayerID}} = pt:read(10002, InPacket),
    case Stat of
        0 ->        % creation success
            Packet = pt:write(10003, 0),
            sender:send(Packet),
            {next_state, wait_for_enter_reply, State#state{player_id = PlayerID}};
        Other ->
            exit({unknown_stat_from_server, Other})
    end;

handle_info({tcp, _Socket, InPacket}, wait_for_enter_reply, State) ->
	{ok, {RoleID, _Name, SceneID, X, Y}} = InPacket,
    NState = State#state{
        role_id = RoleID,
        scene_id = SceneID,
        x = X,
        y = Y
    },
    {next_state, running, NState};

handle_info({tcp, _Socket, InPacket}, running, State) ->
    ?I("Received InPacket = ~w", [InPacket]),
    {next_state, running, State};

handle_info(stop, _StateName, State) ->
    ?I("'stop' received in state ~w", [_StateName]),
    {stop, normal, State};

handle_info({stop, Reason}, _StateName, State) ->
    ?I("'stop' received in state ~w, Reason = ~w", [_StateName, Reason]),
    {stop, Reason, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

