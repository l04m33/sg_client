-ifndef(__CLIENT_HRL__).
-define(__CLIENT_HRL__, true).

-define(CLIENT_ACCOUNT_PREFIX, "k_bot_").
-define(SERVER_KEY, client:get_opt(server_key)).
-define(SERVER_NAME, client:get_opt(server_name)).

-define(HEADER_LENGTH, 4).
-define(RECV_TIMEOUT, 5000).

-record(state, {
    client_id = 0,
    server = {"", 0},       % {ServerIP, ServerPort}
    socket = undefined,
    player_account = "",
    player_id = 0,
    role_id = 0,
    role_level = 0,
    scene_id = 0,
    x = 0,
    y = 0,
    recv_ref = 0,
    test_case = undefined,
    case_state = undefined}).

-endif.

