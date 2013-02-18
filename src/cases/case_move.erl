-module(case_move).

-include("client.hrl").
-include("scene.hrl").

-export([
    prepare/1,
    stop/1,
    handle_server_msg/3,
    handle_timer/2]).

prepare(State) ->
    SceneID = State#state.scene_id,
    SceneInfo = data_scene:get(SceneID),
    RandX = rand_server:rand(1, SceneInfo#scene.column - 1),
    RandY = rand_server:rand(1, SceneInfo#scene.row - 1),
    {NX, NY} = scene:get_movable_point(SceneID, RandX, RandY),
    %gm:jump_to(SceneID, NX, NY),
    gm:jump_to(SceneID, 38, 35),
    {ok, State#state{case_state = no_use}}.


handle_server_msg(State, 11004, Packet) ->
    {ok, {SceneID, X, Y}} = pt:read(11004, Packet),
    scene:enter(SceneID),
    client:case_timer(case_move, 2000, no_use),
    {ok, State#state{scene_id = SceneID, x = X, y = Y}};

handle_server_msg(State, _Cmd, _Packet) ->
    {ok, State}.


handle_timer(State, _NoUse) ->
    SceneID = State#state.scene_id,
    X = State#state.x,
    Y = State#state.y,
    SceneInfo = data_scene:get(SceneID),
    RandX = 
        min(max(1, X + rand_server:rand(-1 * 7, 1 * 7)), 
            SceneInfo#scene.column),
    RandY = 
        min(max(1, Y + rand_server:rand(-1 * 7, 1 * 7)), 
            SceneInfo#scene.row),
    case scene:can_move(SceneID, RandX, RandY) of
        true ->
            MovePacket = pt:write(11000, [{X, Y}, {RandX, RandY}]),
            sender:send(MovePacket),
            StartCheckPacket = pt:write(11003, {X, Y}),
            sender:send(StartCheckPacket),
            EndCheckPacket = pt:write(11003, {RandX, RandY}),
            sender:send(EndCheckPacket),
            client:case_timer(case_move, 2000, _NoUse),
            {ok, State#state{x = RandX, y = RandY}};
        false ->
            client:case_timer(case_move, 2000, _NoUse),
            {ok, State}
    end.


stop(_State) ->
    client:cancel_case_timer(case_move),
    ok.

