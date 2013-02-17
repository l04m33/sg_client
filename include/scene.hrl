-ifndef(__SCENE_HRL__).
-define(__SCENE_HRL__, true).

-record(scene, {
        id = 0,
        type = 0,
        row = 0,
        column = 0,
        level = 0,
        pop_restrict = 0,
        max_level_restrict = 0,
        min_level_restrict = 0,
        times_restrict = 0}).

-record(scene_info, {
		scene_id,
		cols,
		rows,
		type,
		cellcols,
	 	cellrows}).

-define(CELL_HEIGHT, 15).
-define(CELL_WIDTH,  30).

-endif.

