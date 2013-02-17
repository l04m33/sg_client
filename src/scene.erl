-module(scene).

-export([
    enter/1]).

enter(SceneID) ->
    Packet = pt:write(11001, SceneID),
    sender:send(Packet),
    ok.

