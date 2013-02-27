-module(case_competition).

-include("client.hrl").
-include("scene.hrl").
-include("log.hrl").

-export([
    prepare/1,
    stop/1,
    handle_server_msg/3,
    handle_timer/2]).


prepare(State) ->
    case State#state.role_level >= 40 of
        true  -> void;
        false -> gm:add_exp(State#state.role_id, 10000000)
    end,
    %gm:alter_att_and_hp(State#state.role_id, 20000, 20000000),

    %% Wait some time to randomize the robot operations
    RandWaitTime = rand_server:rand(1, 10000),
    receive after RandWaitTime -> ok end,

    ApplyPacket = pt:write(37001, 0),
    sender:send(ApplyPacket),
    {ok, State#state{case_state = prepared}}.


handle_server_msg(State, 11004, Packet) ->
    {ok, {SceneID, X, Y}} = pt:read(11004, Packet),
    scene:enter(SceneID),
    client:case_timer(case_competition, 2000, move),
    case State#state.case_state of
        prepared ->
            {ok, State#state{scene_id = SceneID, x = X, y = Y,
                             case_state = waiting_for_comp_begin}};
        _Other ->
            ?I("_Other = ~w", [_Other]),
            {ok, State#state{scene_id = SceneID, x = X, y = Y}}
    end;

handle_server_msg(State, 11008, Packet) ->
    {ok, {PlayerID, X, Y}} = pt:read(11008, Packet),
    case PlayerID =:= State#state.player_id of
        true ->
            {ok, State#state{x = X, y = Y}};        % My location
        _ ->        % false
            {ok, State}                             % Other's location, can safely ignore
    end;

handle_server_msg(State, 20002, _Packet) ->
    ReadyPacket = pt:write(20002, 0),
    sender:send(ReadyPacket),
    client:case_timer(case_competition, 2000, fight_player),
    {ok, State#state{case_state = fighting}};

handle_server_msg(State, 20005, _Packet) ->
    ?I("Battle ended...."),
    case State#state.case_state of
        fighting ->
            receive after 1000 -> ok end,

            QuitPacket = pt:write(20008, 0),
            sender:send(QuitPacket),
            client:case_timer(case_competition, 2000, move),
            {ok, State#state{case_state = waiting_for_fight}};
        _ ->
            {ok, State}
    end;

handle_server_msg(State, 37003, _Packet) ->
    client:case_timer(case_competition, 2000, move),
    {ok, State#state{case_state = waiting_for_comp_begin}};

handle_server_msg(State, 37007, _Packet) ->
    client:case_timer(case_competition, 2000, auto_comp),
    {ok, State#state{case_state = waiting_for_fight}};

handle_server_msg(State, 37020, _Packet) ->
    LeavePacket = pt:write(37005, 0),
    sender:send(LeavePacket),
    {ok, State#state{case_state = comp_ended}};

handle_server_msg(State, _Cmd, _Packet) ->
    {ok, State}.


handle_timer(State, move) ->
    SceneID = State#state.scene_id,
    X = State#state.x,
    Y = State#state.y,
    SceneInfo = data_scene:get(SceneID),
    RandX = 
        min(max(1, X + rand_server:rand(-1 * 7, 1 * 7)), 
            SceneInfo#scene.column - 1),
    RandY = 
        min(max(1, Y + rand_server:rand(-1 * 7, 1 * 7)), 
            SceneInfo#scene.row - 1),
    case scene:can_move(SceneID, RandX, RandY) of
        true ->
            MovePacket = pt:write(11000, [{X, Y}, {RandX, RandY}]),
            sender:send(MovePacket),
            StartCheckPacket = pt:write(11003, {X, Y}),
            sender:send(StartCheckPacket),
            EndCheckPacket = pt:write(11003, {RandX, RandY}),
            sender:send(EndCheckPacket),
            case State#state.case_state of
                waiting_for_fight ->
                    client:case_timer(case_competition, 2000, auto_comp);
                _ ->
                    client:case_timer(case_competition, 2000, move)
            end,
            {ok, State#state{x = RandX, y = RandY}};
        false ->
            case State#state.case_state of
                waiting_for_fight ->
                    client:case_timer(case_competition, 2000, auto_comp);
                _ ->
                    client:case_timer(case_competition, 2000, move)
            end,
            {ok, State}
    end;

handle_timer(State, auto_comp) ->
    gm:auto_competition(),
    client:case_timer(case_competition, 2000, move),
    {ok, State};

handle_timer(State, fight_player) ->
    AutoPacket = pt:write(20007, 0),
    sender:send(AutoPacket),
    FinishPacket = pt:write(20001, 0),
    sender:send(FinishPacket),
    FinishPacket1 = pt:write(20001, 0),
    sender:send(FinishPacket1),
    client:case_timer(case_competition, 2000, fight_player),
    {ok, State}.


stop(_State) ->
    client:cancel_case_timer(case_competition),
    ok.

