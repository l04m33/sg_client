-module(client).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include("log.hrl").
-include("client.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([
    case_timer/3, 
    cancel_case_timer/1]).

-export([wait_for_login_reply/2]).

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-define(handle_server_info(StateName, Cmd), 
        handle_info({inet_async, _Socket, Ref, {ok, <<Len:16, Cmd:16>>}}, 
                    StateName, #state{recv_ref = Ref} = State)).

-define(handle_server_info(StateName), 
        handle_info({inet_async, _Socket, Ref, {ok, <<Len:16, Cmd:16>>}}, 
                    StateName, #state{recv_ref = Ref} = State)).

-define(HEARTBEAT_TIMEOUT, 60000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

case_timer(CaseName, Timeout, Context) ->
    NRef = msg_timer:schedule_timer(erlang:get({CaseName, timer}), 
                                    Timeout, {timer_case, Context}),
    erlang:put({CaseName, timer}, NRef).

cancel_case_timer(CaseName) ->
    msg_timer:cancel_timer(erlang:get({CaseName, timer})),
    erlang:erase({CaseName, timer}).

start_link(ID, ServerIP, ServerPort, TestCase) ->
    gen_fsm:start_link(?MODULE, [ID, ServerIP, ServerPort, TestCase], []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([ID, ServerIP, ServerPort, TestCase]) ->
    erlang:process_flag(trap_exit, true),
    NState = #state{
        client_id = ID, 
        server = {ServerIP, ServerPort},
        player_account = ?CLIENT_ACCOUNT_PREFIX ++ integer_to_list(ID),
        test_case = TestCase
    },

    ?I("Client started, ServerIP = ~s, ServerPort = ~w", [ServerIP, ServerPort]),
    self() ! connect,
    ?SINFO(idle),
    {ok, idle, NState}.


wait_for_login_reply(timeout, State) ->
    self() ! {stop, login_timed_out},
    ?SINFO(disconnected),
    {next_state, disconnected, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.


handle_info(connect, idle, State) ->
    ?I("Connecting...."),
    {IP, Port} = State#state.server,
	case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            sender:init_sender(Socket),
            NState = State#state{socket = Socket},
            self() ! login,
            ?SINFO(connected),
            {next_state, connected, NState};
        {error, Reason} ->
            ?E("Client ~w failed to connect, IP = ~s, Port = ~w, Reason = ~w",
               [State#state.client_id, IP, Port, Reason]),
            self() ! {stop, Reason},
            ?SINFO(disconnected),
            {next_state, disconnected, State}
    end;

handle_info(login, connected, State) ->
    Account = State#state.player_account,
    Packet = pt:write(10000, Account),
    sender:send(Packet),
    NState = async_recv(State),
    ?SINFO(wait_for_login_reply),
    {next_state, wait_for_login_reply, NState, 5000};

?handle_server_info(wait_for_login_reply, 10000) ->
    InPacket = read_msg_body(State, Len, 10000),
    {ok, {Stat, PlayerID}} = pt:read(10000, InPacket),
    case Stat of
        2 ->        % login success
            Packet = pt:write(10003, 0),
            sender:send(Packet),
            NState = async_recv(State),
            ?SINFO(wait_for_enter_reply),
            {next_state, wait_for_enter_reply, NState#state{player_id = PlayerID}};
        1 ->        % create new role
            NRoleID = rand_server:rand(1, 6),
            ?I("New role ID: ~w", [NRoleID]),
            Packet = pt:write(10002, {NRoleID, State#state.player_account}),
            sender:send(Packet),
            NState = async_recv(State),
            ?SINFO(wait_for_create_role_reply),
            {next_state, wait_for_create_role_reply, NState};
        Other ->
            exit({unknown_stat_from_server, Other})
    end;

?handle_server_info(wait_for_create_role_reply, 10002) ->
    InPacket = read_msg_body(State, Len, 10002),
    {ok, {Stat, PlayerID}} = pt:read(10002, InPacket),
    case Stat of
        0 ->        % creation success
            Packet = pt:write(10003, 0),
            sender:send(Packet),
            NState = async_recv(State),
            ?SINFO(wait_for_enter_reply),
            {next_state, wait_for_enter_reply, NState#state{player_id = PlayerID}};
        Other ->
            exit({unknown_stat_from_server, Other})
    end;

?handle_server_info(wait_for_enter_reply, 10003) ->
    InPacket = read_msg_body(State, Len, 10003),
	{ok, {RoleID, _Name, SceneID, X, Y}} = pt:read(10003, InPacket),
    ?I("RoleID = ~w, Name = ~ts, SceneID = ~w, X = ~w, Y = ~w",
       [RoleID, _Name, SceneID, X, Y]),
    NState0 = State#state{
        role_id = RoleID,
        scene_id = SceneID,
        x = X,
        y = Y
    },

    scene:enter(SceneID),
    HeartbeatTimerRef = 
        msg_timer:schedule_timer(none, ?HEARTBEAT_TIMEOUT, timer_heartbeat),
    erlang:put(heartbeat_timer_ref, HeartbeatTimerRef),

    NState1 = async_recv(NState0),
    NState = init_case(NState1),
    ?SINFO(running),
    {next_state, running, NState};

?handle_server_info(running) ->
    InPacket = read_msg_body(State, Len, Cmd),
    ?I("Received Len = ~w, Cmd = ~w, InPacket = ~w", [Len, Cmd, InPacket]),

    Case = State#state.test_case,
    {ok, NState0} = Case:handle_server_msg(State, Cmd, InPacket),

    NState = async_recv(NState0, -1),
    {next_state, running, NState};

handle_info(timer_heartbeat, running, State) ->
    ?I("Heartbeat triggered"),
    Packet = pt:write(10005, 0),
    sender:send(Packet),
    NewTimerRef = 
        msg_timer:schedule_timer(erlang:get(heartbeat_timer_ref),
                                 ?HEARTBEAT_TIMEOUT, timer_heartbeat),
    erlang:put(heartbeat_timer_ref, NewTimerRef),
    {next_state, running, State};

handle_info({timer_case, Context}, running, State) ->
    ?I("timer_case triggered, Context = ~w", [Context]),
    Case = State#state.test_case,
    {ok, NState} = Case:handle_timer(State, Context),
    {next_state, running, NState};

handle_info(stop, _StateName, State) ->
    ?I("'stop' received in state ~w", [_StateName]),
    {stop, normal, State};

handle_info({stop, Reason}, _StateName, State) ->
    ?I("'stop' received in state ~w, Reason = ~w", [_StateName, Reason]),
    {stop, Reason, State};

handle_info({inet_async, _Socket, _Ref, {error, ErrReason}}, _StateName, State) ->
    {stop, {socket_error, ErrReason}, State};

handle_info(_Info, StateName, State) ->
    ?E("received in state ~w: ~w", [StateName, _Info]),
    {next_state, StateName, State}.


terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

async_recv(State) ->
    Ref = async_recv(State#state.socket, ?HEADER_LENGTH, ?RECV_TIMEOUT),
    State#state{recv_ref = Ref}.

async_recv(State, Timeout) ->
    Ref = async_recv(State#state.socket, ?HEADER_LENGTH, Timeout),
    State#state{recv_ref = Ref}.

async_recv(Sock, Length, Timeout) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> 
			?E("~w", [Reason]),
            exit({socket_error, Reason});
        {ok, Res} -> 
            Res
    end.

read_msg_body(State, Len, _Cmd) ->
    case Len > ?HEADER_LENGTH of
        true ->
            Ref = async_recv(State#state.socket, Len - ?HEADER_LENGTH, ?RECV_TIMEOUT),
            receive 
                {inet_async, _, Ref, {ok, Body}} ->
                    Body;
                _Other ->
                    exit({socket_error, _Other})
            end;
        _ ->        % false
            <<>>
    end.

init_case(State) ->
    Case = State#state.test_case,
    case Case:prepare(State) of
        {ok, NState} ->
            NState;
        _Other ->
            exit({case_prepare_failed, _Other})
    end.

