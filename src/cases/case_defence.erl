-module(case_defence).

-include("client.hrl").
-include("scene.hrl").
-include("log.hrl").

-define(DEFENCE_SCENE_ID, 3500).

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
    gm:alter_att_and_hp(State#state.role_id, 200000, 20000000),
    SceneInfo = data_scene:get(?DEFENCE_SCENE_ID),
    RandX = rand_server:rand(1, SceneInfo#scene.column - 1),
    RandY = rand_server:rand(1, SceneInfo#scene.row - 1),
    {NX, NY} = scene:get_movable_point(?DEFENCE_SCENE_ID, RandX, RandY),
    %gm:jump_to(?DEFENCE_SCENE_ID, NX, NY),
    gm:jump_to(?DEFENCE_SCENE_ID, 291, 35),
    {ok, State#state{case_state = prepared}}.


handle_server_msg(State, 11004, Packet) ->
    {ok, {SceneID, X, Y}} = pt:read(11004, Packet),
    scene:enter(SceneID),
    case State#state.case_state of
        prepared ->
            client:case_timer(case_defence, 2000, request_mon_coord),
            {ok, State#state{scene_id = SceneID, x = X, y = Y,
                             case_state = waiting_for_mon_coord}};
        {jumping_for_fight, _MonID} ->
            {ok, State#state{scene_id = SceneID, x = X, y = Y}};
        _Other ->
            ?I("_Other = ~w", [_Other]),
            {ok, State}
    end;

handle_server_msg(State, 11001, _Packet) ->
    case State#state.case_state of
        {jumping_for_fight, MonID} ->
            FightPacket = pt:write(11404, {State#state.scene_id, MonID}),
            sender:send(FightPacket),
            {ok, State};
        _ ->
            {ok, State}
    end;

handle_server_msg(State, 10999, _Packet) ->
    client:case_timer(case_defence, 2000, request_mon_coord),
    {ok, State#state{case_state = waiting_for_mon_coord}};

handle_server_msg(State, 20000, _Packet) ->
    ReadyPacket = pt:write(20000, 0),
    sender:send(ReadyPacket),
    client:case_timer(case_defence, 1000, fight_monster),
    {ok, State#state{case_state = fighting}};

handle_server_msg(State, 20005, _Packet) ->
    ?I("Battle ended...."),
    case State#state.case_state of
        fighting ->
            QuitPacket = pt:write(20008, 0),
            sender:send(QuitPacket),
            client:case_timer(case_defence, 2000, request_mon_coord),
            {ok, State#state{case_state = waiting_for_mon_coord}};
        _ ->
            {ok, State}
    end;

handle_server_msg(State, 40007, Packet) ->
    {ok, {MonID, MonX, MonY}} = pt:read(40007, Packet),
    gm:jump_to(State#state.scene_id, MonX, MonY),
    {ok, State#state{case_state = {jumping_for_fight, MonID}}};

handle_server_msg(State, _Cmd, _Packet) ->
    {ok, State}.


handle_timer(State, request_mon_coord) ->
    Packet = pt:write(40007, 0),
    sender:send(Packet),
    client:case_timer(case_defence, 2000, request_mon_coord),
    {ok, State};

handle_timer(State, fight_monster) ->
    AutoPacket = pt:write(20007, 0),
    sender:send(AutoPacket),
    FinishPacket = pt:write(20001, 0),
    sender:send(FinishPacket),
    FinishPacket1 = pt:write(20001, 0),
    sender:send(FinishPacket1),
    client:case_timer(case_defence, 1000, fight_monster),
    {ok, State}.


stop(_State) ->
    client:cancel_case_timer(case_defence),
    ok.

