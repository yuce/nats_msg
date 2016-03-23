#! /usr/bin/env escript

%%! -pa _build/default/lib/nats_msg/ebin

-mode(compile).
-define(MSEC2SEC(MSec), (MSec / 1000000)).
-define(OPS(MSec), (1000000 / (MSec / 1000000))).
-define(TIMES, 1000000).

main([]) ->
    nats_msg:init(),
    Benchs = [{"decode +OK message 1m times", fun() -> decode_ok() end},
              {"decode all +OK, PING message 1m times", fun() -> decode_all() end},
              {"decode CONNECT message 1m times", fun() -> decode_connect() end},
              {"decode MSG message 1m times", fun() -> decode_msg() end},
              {"encode +OK message 1m times", fun() -> encode_ok() end},
              {"encode PUB message 1m times", fun() -> encode_pub() end},
              {"encode MSG message 1m times", fun() -> encode_msg() end}],
    F = fun({Text, Fun}) ->
        io:format("Benching: ~s~n", [Text]),
        {Time, ok} = timer:tc(Fun),
        TimeSecs = ?MSEC2SEC(Time),
        io:format("    Time: ~p secs~n", [TimeSecs]),
        io:format("    OPS : ~f~n", [?OPS(Time)])
    end,
    lists:foreach(F, Benchs).

decode_ok() ->
    F = fun() ->
        nats_msg:decode(<<"+OK\r\n">>)
    end,
    times(F, ?TIMES).

decode_connect() ->
    F = fun() ->
        nats_msg:decode(<<"CONNECT {\"name\":\"sample-client\",\"verbose\":true}\r\n">>)
    end,
    times(F, ?TIMES).

decode_msg() ->
    F = fun() ->
        nats_msg:decode(<<"MSG FOO.BAR 9 INBOX.34 13\r\nHello, World!\r\n">>)
    end,
    times(F, ?TIMES).

decode_all() ->
    F = fun() ->
        nats_msg:decode_all(<<"+OK\r\nPING\r\nM">>)
    end,
    times(F, ?TIMES).

encode_ok() ->
    F = fun() ->
        nats_msg:ok()
    end,
    times(F, ?TIMES).

encode_pub() ->
    F = fun() ->
        nats_msg:pub(<<"FRONT.DOOR">>, <<"INBOX.22">>, <<"Knock Knock">>)
    end,
    times(F, ?TIMES).

encode_msg() ->
    F = fun() ->
        nats_msg:msg(<<"MSG FOO.BAR">>, <<"9">>, <<"INBOX.34">>, <<"Hello, World!">>)
    end,
    times(F, ?TIMES).

times(_, 0) ->
    ok;

times(F, N) ->
    F(),
    times(F, N - 1).
