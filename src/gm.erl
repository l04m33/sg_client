-module(gm).

-export([
    jump_to/3]).

jump_to(SceneID, X, Y) ->
    GMStr = io_lib:format("gm:22 ~p ~p ~p", [SceneID, X, Y]),
    Packet = pt:write(16000, GMStr),
    sender:send(Packet),
    ok.

