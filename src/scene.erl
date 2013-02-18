-module(scene).

-include("scene.hrl").

-export([
    init_masks/0,
    enter/1,
    can_move/3,
    get_movable_point/3]).

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

get_movable_point(SceneID, X, Y) ->
	Scene = data_scene:get(SceneID),
	Row   = Scene#scene.row,
	Col   = Scene#scene.column,	
	
	case can_move(SceneID, X, Y) of
		true  -> {X, Y};
		false -> get_movable_point(SceneID, Row, Col, X, Y, 1)
	end.

get_movable_point(SceneID, Row, Col, X, Y, N) ->
	Points = get_movable_point_list(X, Y, N),
	case get_movable_point_1(SceneID, Row, Col, Points) of
		{true, Point} -> Point;
		false ->
			get_movable_point(SceneID, Row, Col, X, Y, N + 1)
	end.

get_movable_point_1(_SceneID, _Row, _Col, []) -> false;
get_movable_point_1(SceneID, Row, Col, [{X, Y} | Rest]) ->
	case X =< Col - 1 andalso 
		 Y =< Row - 1 andalso 
		 X >= 0 andalso Y >= 0 andalso can_move(SceneID, X, Y) of
		
		true  -> {true, {X, Y}};
		false -> get_movable_point_1(SceneID, Row, Col, Rest)
	end.
	
get_movable_point_list(X, Y, N) ->
	TopList = 
		lists:map(fun(I) -> {X - N + I, Y - N} end, lists:seq(0, 2 * N)),
	BottomList = 
		lists:map(fun(I) -> {X - N + I, Y + N} end, lists:seq(0, 2 * N)),
	LeftList = 
		lists:map(fun(I) -> {X - N, Y - N + 1 + I} end, lists:seq(0, 2 * (N - 1))),
	RightList = 
		lists:map(fun(I) -> {X + N, Y - N + 1 + I} end, lists:seq(0, 2 * (N - 1))),
	
	TopList ++ BottomList ++ LeftList ++ RightList.

