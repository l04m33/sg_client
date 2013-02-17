-module(msg_timer).

-export([
    schedule_timer/3, 
    cancel_timer/1]).

schedule_timer(OldTimerRef, TimeOut, Msg) ->
    cancel_timer(OldTimerRef),
    NewTRef = erlang:send_after(TimeOut, self(), Msg),
    {NewTRef, Msg}.

cancel_timer(none) -> none;
cancel_timer(undefined) -> none;
cancel_timer({OldTimerRef, Msg}) ->
    erlang:cancel_timer(OldTimerRef),
    receive
        Msg -> ok
        after 0 -> ok
    end,
    none.

