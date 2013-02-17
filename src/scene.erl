-module(scene).

-include("scene.hrl").

-export([
    init_masks/0,
    enter/1,
    can_move/3]).

init_masks() ->
    ets:new(ets_mask_info,  
            [public, named_table, set, 
             {read_concurrency, true}]),
	ets:new(ets_scene_info, 
            [public, named_table, set, 
             {keypos, #scene_info.scene_id},
             {read_concurrency, true}]),
    SceneList = data_scene:get_id_list(),
    init_masks(SceneList).

init_masks([SceneID | Rest]) ->
    MaskTuple = list_to_tuple([SceneID | data_mask:get(SceneID)]),
    ets:insert(ets_mask_info, MaskTuple),
    #scene{row = R, column = C, type = T} = data_scene:get(SceneID),
	CellRow = (R + ?CELL_HEIGHT - 1) div ?CELL_HEIGHT,
	CellCol = (C + ?CELL_WIDTH  - 1) div ?CELL_WIDTH,
    ets:insert(ets_scene_info, 
               #scene_info{
                    scene_id = SceneID, 
                    cellcols = CellCol, 
                    cellrows = CellRow, 
                    cols = C, 
                    rows = R, 
                    type = T}),
    init_masks(Rest);
init_masks([]) ->
    ok.

enter(SceneID) ->
    Packet = pt:write(11001, SceneID),
    sender:send(Packet),
    ok.

can_move(SceneID, X, Y) ->
	[#scene_info{cols = C} | _] = ets:lookup(ets_scene_info, SceneID),
	ets:lookup_element(ets_mask_info, SceneID, Y * C + X + 2) =/= 49.

