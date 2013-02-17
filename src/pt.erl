-module(pt).

-include("log.hrl").
-include("client.hrl").

-export([route/1,
         pack/2,
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


read(10000, <<Stat:8, PlayerID:32>>) ->
    {ok, {Stat, PlayerID}};

read(10002, <<Stat:8, PlayerID:32>>) ->
    {ok, {Stat, PlayerID}};

read(10003, <<RoleID:32, Rest/binary>>) ->
	{Name, Bin} = read_string(Rest),
	<<SceneID:16, X:16, Y:16, _/binary>> = Bin,
	{ok, {RoleID, Name, SceneID, X, Y}}.


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
    pack(10003, <<0:8>>).


str_md5(L) ->
    MD5Bin = erlang:md5(L),
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(MD5Bin)]).

