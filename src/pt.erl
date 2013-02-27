-module(pt).

-include("log.hrl").
-include("client.hrl").

-export([route/1,
         pack/2,
         write_string/1,
         read_string/1,
         write_pos_list/1,
         read/2,
         write/2]).

route(<<ProtoID:16, Rest/binary>>) ->
    read(ProtoID, Rest).

pack(ProtoID, Payload) ->
    Len = byte_size(Payload) + 6,
    Seq = sender:get_and_rotate_seq(),
    <<Len:16, ProtoID:16, Seq:16, Payload/binary>>.

write_string(Str) ->
	BinStr = list_to_binary(Str),
    Len = byte_size(BinStr),
    <<Len:16, BinStr/binary>>.

read_string(Bin) ->
    case Bin of
        <<Len:16, Bin1/binary>> ->
            case Bin1 of
                <<Str:Len/binary-unit:8, Rest/binary>> ->
                    {binary_to_list(Str), Rest};
                _R1 ->
                    {[],<<>>}
            end;
        _R1 ->
            {[],<<>>}
    end.

write_pos_list(PosList) ->
    write_pos_list(PosList, <<>>, 0).

write_pos_list([{X, Y} | Rest], AccBin, Len) ->
    write_pos_list(Rest, <<AccBin/binary, X:16, Y:16>>, Len + 1);
write_pos_list([], AccBin, Len) ->
    <<Len:16, AccBin/binary>>.


read(10000, <<Stat:8, PlayerID:32>>) ->
    {ok, {Stat, PlayerID}};

read(10002, <<Stat:8, PlayerID:32>>) ->
    {ok, {Stat, PlayerID}};

read(10003, <<RoleID:32, Rest/binary>>) ->
	{Name, Bin} = read_string(Rest),
	<<SceneID:16, X:16, Y:16, _:672, RoleLevel:16>> = Bin,
	{ok, {RoleID, Name, SceneID, X, Y, RoleLevel}};

read(11004, <<SceneID:16, X:16, Y:16>>) ->
    {ok, {SceneID, X, Y}};

read(11008, <<PlayerID:32, X:16, Y:16>>) ->
    {ok, {PlayerID, X, Y}};

read(40007, <<MonID:32, MonX:16, MonY:16>>) ->
    {ok, {MonID, MonX, MonY}};

read(guard, _) ->
    guard.


write(10000, Account) ->
    AccountBin = list_to_binary(Account),
    AccountLen = byte_size(AccountBin),
    
    {M, S, _} = erlang:now(),
    Now = M * 1000000 + S,
    ServerTicket = str_md5([integer_to_list(Now), Account, ?SERVER_KEY]),
    ?I("Ticket = ~w", [ServerTicket]),
    
    TicketLen = byte_size(ServerTicket),
    Payload = <<AccountLen:16, AccountBin/binary, 0:8, Now:32, TicketLen:16, ServerTicket/binary>>,
    pack(10000, Payload);

write(10002, {RoleID, RoleName}) ->
	RoleNameBin = write_string(RoleName),
	ServerNameBin = write_string(?SERVER_NAME),
    Payload = <<RoleID:16, RoleNameBin/binary, ServerNameBin/binary>>,
    pack(10002, Payload);

write(10003, _NoUse) ->
    pack(10003, <<0:8>>);

write(10005, _NoUse) ->
    pack(10005, <<0:8>>);

write(11000, PosList) ->
    Payload = write_pos_list(PosList),
    pack(11000, Payload);

write(11001, SceneID) ->
    pack(11001, <<SceneID:16>>);

write(11003, {X, Y}) ->
    pack(11003, <<X:16, Y:16>>);

write(11404, {SceneID, MonID}) ->
    pack(11404, <<SceneID:16, MonID:16>>);

write(16000, Str) ->
    Payload = write_string(Str),
    pack(16000, Payload);

write(20000, _NoUse) ->
    pack(20000, <<0:8>>);

write(20001, Cmd) ->
    pack(20001, <<Cmd:32>>);

write(20002, _NoUse) ->
    pack(20002, <<0:8>>);

write(20007, _NoUse) ->
    pack(20007, <<0:8>>);

write(20008, _NoUse) ->
    pack(20008, <<0:8>>);

write(20100, MonID) ->
    pack(20100, <<MonID:32>>);

write(37001, NPCID) ->
    pack(37001, <<NPCID:16>>);

write(37005, _NoUse) ->
    pack(37005, <<_NoUse:8>>);

write(40007, _NoUse) ->
    pack(40007, <<0:8>>);

write(guard, _) ->
    guard.


str_md5(L) ->
    MD5Bin = erlang:md5(L),
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(MD5Bin)]).

