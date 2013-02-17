-module(data_scene).

-compile(export_all).

-include("scene.hrl").

%% get all scene id list
get_id_list() ->
	[1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 2000, 3000, 3100, 3200, 3201, 3202, 3300, 3301, 3302, 3400, 3500, 3600, 3700].


%%================================================
%% 获取玩家第一次进入游戏可以进入的地图
get_init_access() ->
	[1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 2000, 3000, 3100, 3200, 3201, 3202, 3300, 3301, 3302, 3400, 3500, 3600, 3700].


%%================================================
%% 获取世界boss的地图，地图类型为?SCENE_ARENA2
get_boss_scene() ->
	[3000].


%%================================================
%% 获取爬塔的地图
get_tower_scene() ->
	[3100].


%%================================================
%% 获取所有的副本地图id
get_all_dungeon() ->
	[1200, 1400, 1600, 1800, 2000, 3600].


%%================================================
%% 获取所有的副本地图id
get_all_territory_war() ->
	[3300, 3301, 3302].


%%================================================
%% 获取所有的副本地图id
get(1000) ->
	#scene{
		id     = 1000,
		type   = 1,
		row    = 190,
		column = 329
	   };

get(1100) ->
	#scene{
		id     = 1100,
		type   = 2,
		row    = 180,
		column = 188
	   };

get(1200) ->
	#scene{
		id     = 1200,
		type   = 3,
		row    = 111,
		column = 178
	   };

get(1300) ->
	#scene{
		id     = 1300,
		type   = 2,
		row    = 167,
		column = 167
	   };

get(1400) ->
	#scene{
		id     = 1400,
		type   = 3,
		row    = 123,
		column = 117
	   };

get(1500) ->
	#scene{
		id     = 1500,
		type   = 2,
		row    = 150,
		column = 184
	   };

get(1600) ->
	#scene{
		id     = 1600,
		type   = 3,
		row    = 100,
		column = 100
	   };

get(1700) ->
	#scene{
		id     = 1700,
		type   = 2,
		row    = 150,
		column = 184
	   };

get(1800) ->
	#scene{
		id     = 1800,
		type   = 3,
		row    = 114,
		column = 167
	   };

get(2000) ->
	#scene{
		id     = 2000,
		type   = 3,
		row    = 55,
		column = 100
	   };

get(3000) ->
	#scene{
		id     = 3000,
		type   = 6,
		row    = 27,
		column = 100
	   };

get(3100) ->
	#scene{
		id     = 3100,
		type   = 7,
		row    = 36,
		column = 64
	   };

get(3200) ->
	#scene{
		id     = 3200,
		type   = 8,
		row    = 68,
		column = 101
	   };

get(3201) ->
	#scene{
		id     = 3201,
		type   = 8,
		row    = 68,
		column = 101
	   };

get(3202) ->
	#scene{
		id     = 3202,
		type   = 8,
		row    = 68,
		column = 101
	   };

get(3300) ->
	#scene{
		id     = 3300,
		type   = 9,
		row    = 101,
		column = 162
	   };

get(3301) ->
	#scene{
		id     = 3301,
		type   = 9,
		row    = 101,
		column = 162
	   };

get(3302) ->
	#scene{
		id     = 3302,
		type   = 9,
		row    = 101,
		column = 162
	   };

get(3400) ->
	#scene{
		id     = 3400,
		type   = 10,
		row    = 68,
		column = 107
	   };

get(3500) ->
	#scene{
		id     = 3500,
		type   = 11,
		row    = 190,
		column = 329
	   };

get(3600) ->
	#scene{
		id     = 3600,
		type   = 3,
		row    = 114,
		column = 167
	   };

get(3700) ->
	#scene{
		id     = 3700,
		type   = 12,
		row    = 72,
		column = 97
	   }.


%%================================================
%% 获取进入该地图的默认点
get_default_xy(1000) -> {320, 184};

get_default_xy(1100) -> {26, 27};

get_default_xy(1200) -> {8, 79};

get_default_xy(1300) -> {16, 104};

get_default_xy(1400) -> {107, 99};

get_default_xy(1500) -> {18, 18};

get_default_xy(1600) -> {92, 93};

get_default_xy(1700) -> {18, 18};

get_default_xy(1800) -> {74, 105};

get_default_xy(2000) -> {85, 46};

get_default_xy(3000) -> {15, 19};

get_default_xy(3100) -> {9, 29};

get_default_xy(3200) -> {9, 56};

get_default_xy(3201) -> {9, 56};

get_default_xy(3202) -> {9, 56};

get_default_xy(3300) -> {145, 83};

get_default_xy(3301) -> {145, 83};

get_default_xy(3302) -> {145, 83};

get_default_xy(3400) -> {6, 59};

get_default_xy(3500) -> {155, 97};

get_default_xy(3600) -> {74, 105};

get_default_xy(3700) -> {73, 17}.


%%================================================
%% 获取该地图的进入次数限制(0代表无限制)
get_tickets(1200) -> 4;

get_tickets(1400) -> 4;

get_tickets(1600) -> 4;

get_tickets(1800) -> 4;

get_tickets(2000) -> 2;

get_tickets(3600) -> 2.


%%================================================
%% 获取该地图的进入次数限制(0代表无限制)
get_can_guaji_map() -> [1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 3000, 3500, 2000, 3600].


%%================================================
%% 获取该地图的名字
get_scene_name(1000) -> "洛阳";

get_scene_name(1100) -> "汜水";

get_scene_name(1200) -> "虎牢关";

get_scene_name(1300) -> "官渡";

get_scene_name(1400) -> "乌巢";

get_scene_name(1500) -> "襄阳";

get_scene_name(1600) -> "长坂坡";

get_scene_name(1700) -> "江夏";

get_scene_name(1800) -> "赤壁";

get_scene_name(3201) -> "比武场";

get_scene_name(3202) -> "比武场";

get_scene_name(3000) -> "世界BOSS";

get_scene_name(3400) -> "挑战魂将";

get_scene_name(3500) -> "群魔乱舞";

get_scene_name(3100) -> "战神塔";

get_scene_name(2000) -> "汜水关";

get_scene_name(3300) -> "领地战";

get_scene_name(3200) -> "比武场";

get_scene_name(3600) -> "金银窟";

get_scene_name(3700) -> "帮派领地";

get_scene_name(3301) -> "领地战";

get_scene_name(3302) -> "领地战".


%%================================================
%% 获取人物在场景中的速度
%% 普通移动
get_scene_spee(1) -> 200;

%% 运美女
get_scene_spee(2) -> 130;

%% 飞行
get_scene_spee(3) -> 250;

%% 比武加速
get_scene_spee(4) -> 200;

%% 比武移动
get_scene_spee(5) -> 140;

%% 新手运美女
get_scene_spee(6) -> 190.


%%================================================
%% 获取公会活动的地图
get_guild_hunting_scene() -> 3700.


%%================================================
%% 检查地图能否飞行
is_can_fly(1000) -> true;

is_can_fly(1100) -> true;

is_can_fly(1200) -> false;

is_can_fly(1300) -> true;

is_can_fly(1400) -> false;

is_can_fly(1500) -> true;

is_can_fly(1600) -> false;

is_can_fly(1700) -> true;

is_can_fly(1800) -> false;

is_can_fly(3201) -> false;

is_can_fly(3202) -> false;

is_can_fly(3000) -> false;

is_can_fly(3400) -> false;

is_can_fly(3500) -> true;

is_can_fly(3100) -> false;

is_can_fly(2000) -> false;

is_can_fly(3300) -> false;

is_can_fly(3200) -> false;

is_can_fly(3600) -> false;

is_can_fly(3700) -> true;

is_can_fly(3301) -> false;

is_can_fly(3302) -> false.


%%================================================
