-module(gm).

-export([
    jump_to/3,
    add_exp/2,
    alter_att_and_hp/3,
    auto_competition/0]).

jump_to(SceneID, X, Y) ->
    GMStr = io_lib:format("gm:22 ~w ~w ~w", [SceneID, X, Y]),
    Packet = pt:write(16000, GMStr),
    sender:send(Packet),
    ok.

add_exp(RoleID, Exp) ->
    GMStr = io_lib:format("gm:10 ~w ~w", [RoleID, Exp]),
    Packet = pt:write(16000, GMStr),
    sender:send(Packet),
    ok.

alter_att_and_hp(RoleID, Att, HP) ->
    GMStr = io_lib:format("gm:16 ~w ~w ~w", [RoleID, Att, HP]),
    Packet = pt:write(16000, GMStr),
    sender:send(Packet),
    ok.

auto_competition() ->
    GMStr = "gm:86",
    Packet = pt:write(16000, GMStr),
    sender:send(Packet),
    ok.

