-module(sender).

-export([init_sender/1,
         get_seq/0,
         get_and_rotate_seq/0,
         send/1]).

init_sender(Socket) ->
    erlang:put(sender_info, {Socket, 65500}),
    ok.

get_seq() ->
    {_, Seq} = erlang:get(sender_info),
    Seq.

get_and_rotate_seq() ->
    {Socket, Seq} = erlang:get(sender_info),
    NewSeq = case Seq - 1 of
        -1 -> 65535;
        N  -> N
    end,
    erlang:put(sender_info, {Socket, NewSeq}),
    Seq.

send(Bin) ->
    {Socket, _Seq} = erlang:get(sender_info),
    send(Socket, Bin).

send(Socket, Bin) ->
    gen_tcp:send(Socket, Bin).

