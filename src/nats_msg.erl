-module(nats_msg).
-author("Yuce Tekol").

-export([encode/1,
         encode/3,
         decode/1]).

-export([ping/0,
         pong/0,
         ok/0,
         err/1,
         info/1,
         connect/1,
         pub/1,
         pub/3,
         sub/2,
         sub/3,
         unsub/1,
         unsub/2,
         msg/2,
         msg/4]).

-define(SEP, <<" ">>).
-define(NL, <<"\r\n">>).
-define(SEPLIST, [<<" ">>, <<"\t">>]).

%% == Encode API

ping() -> encode(ping).
pong() -> encode(pong).
ok() -> encode(ok).
err(Msg) -> encode({err, Msg}).
info(Info) -> encode({info, Info}).
connect(Info) -> encode({connect, Info}).

pub(Subject) -> encode({pub, {Subject, undefined, 0}, <<>>}).
pub(Subject, ReplyTo, Payload) ->
    encode({pub, {Subject, ReplyTo, byte_size(Payload)}, Payload}).

sub(Subject, Sid) -> encode({sub, {Subject, undefined, Sid}}).
sub(Subject, QueueGrp, Sid) -> encode({sub, {Subject, QueueGrp, Sid}}).

unsub(Sid) -> encode({unsub, {Sid, undefined}}).
unsub(Sid, MaxMsg) -> encode({unsub, {Sid, MaxMsg}}).

msg(Subject, Sid) ->
    encode({msg, {Subject, Sid, undefined, 0}, <<>>}).
msg(Subject, Sid, ReplyTo, Payload) ->
    encode({msg, {Subject, Sid, ReplyTo, byte_size(Payload)}, Payload}).

encode(ping) -> encode(ping, [], undefined);
encode(pong) -> encode(pong, [], undefined);
encode(ok) -> encode(ok, [], undefined);

encode({err, unknown_protocol}) ->
    encode(err, [<<"'Unknown Protocol Operation'">>], undefined);

encode({err, auth_violation}) ->
    encode(err, [<<"'Authorization Violation'">>], undefined);

encode({err, auth_timeout}) ->
    encode(err, [<<"'Authorization Timeout'">>], undefined);

encode({err, parser_error}) ->
    encode(err, [<<"'Parser Error'">>], undefined);

encode({err, stale_connection}) ->
    encode(err, [<<"'Stale Connection'">>], undefined);

encode({err, slow_consumer}) ->
    encode(err, [<<"'Slow Consumer'">>], undefined);

encode({err, max_payload}) ->
    encode(err, [<<"'Maximum Payload Exceeded'">>], undefined);

encode({err, invalid_subject}) ->
    encode(err, [<<"'Invalid Subject'">>], undefined);

encode({err, ErrMsg}) ->
    encode(err, [ErrMsg], undefined);

encode({info, Info}) ->
    BinInfo = jsx:encode(Info),
    encode(info, [BinInfo], undefined);

encode({connect, Info}) ->
    BinInfo = jsx:encode(Info),
    encode(connect, [BinInfo], undefined);

encode({pub, {Subject, ReplyTo, Bytes}, Payload}) ->
    Params = case ReplyTo of
        undefined -> [Subject, integer_to_binary(Bytes)];
        _ -> [Subject, ReplyTo, integer_to_binary(Bytes)]
    end,
    encode(pub, Params, Payload);

encode({sub, {Subject, QueueGrp, Sid}}) ->
    Params = case QueueGrp of
        undefined ->
            [Subject, Sid];
        _ ->
            [Subject, QueueGrp, Sid]
    end,
    encode(sub, Params, undefined);

encode({unsub, {Subject, MaxMsg}}) ->
    Params = case MaxMsg of
        undefined ->
            [Subject];
        M when M > 0 ->
            [Subject, integer_to_binary(M)]
    end,
    encode(unsub, Params, undefined);

encode({msg, {Subject, Sid, ReplyTo, Bytes}, Payload}) ->
    Params = case ReplyTo of
        undefined -> [Subject, Sid, integer_to_binary(Bytes)];
        _ -> [Subject, Sid, ReplyTo, integer_to_binary(Bytes)]
    end,
    encode(msg, Params, Payload).

-spec encode(Name :: atom() | binary(),
              Params :: [binary()],
              Payload :: binary()) ->
    Message :: binary().

encode(Name, Params, Payload) when is_atom(Name) ->
    encode(name_to_bin(Name), Params, Payload);

encode(Name, Params, Payload) ->
    Encoded = encode_message(Name, Params, Payload),
    iolist_to_binary(Encoded).

% == Decode API

-spec decode(Binary :: binary()) ->
    {Messages :: [term()], Remaining :: binary()}.

decode(Binary) ->
    {Messages, Rem} = extract_line(Binary, undefined, [], undefined),
    {lists:reverse(Messages), Rem}.

%% == Internal

name_to_bin(info) -> <<"INFO">>;
name_to_bin(connect) -> <<"CONNECT">>;
name_to_bin(pub) -> <<"PUB">>;
name_to_bin(sub) -> <<"SUB">>;
name_to_bin(unsub) -> <<"UNSUB">>;
name_to_bin(msg) -> <<"MSG">>;
name_to_bin(ping) -> <<"PING">>;
name_to_bin(pong) -> <<"PONG">>;
name_to_bin(ok) -> <<"+OK">>;
name_to_bin(err) -> <<"-ERR">>.

encode_message(Name, Params, Payload) ->
    R1 = [Name],
    RevParams = lists:reverse(Params),
    R2 = case RevParams of
        [] -> R1;
        [H | Rest] ->
            F = fun(P, Acc) -> [P, ?SEP | Acc] end,
            [lists:foldl(F, [H], Rest), ?SEP | R1]
    end,
    R3 = case Payload of
        undefined ->
            R2;
        <<>> ->
            [?NL | R2];
        _ ->
            [Payload, ?NL | R2]
    end,
    lists:reverse([?NL | R3]).

extract_line(Bin, undefined, MsgAcc, _) ->
    case binary:match(Bin, ?NL) of
        nomatch ->
            {MsgAcc, Bin};
        {Pos, Len} ->
            Line = binary:part(Bin, {0, Pos}),
            Rest = binary:part(Bin, {Pos + Len, byte_size(Bin) - (Pos + Len)}),
            {NewMsgAcc, NewPayloadSize, Hold} = extract_msg(Line, MsgAcc),
            case Hold of
                true ->
                    extract_line(Rest, NewPayloadSize, NewMsgAcc, <<Line/binary, ?NL/binary>>);
                _ ->
                    extract_line(Rest, NewPayloadSize, NewMsgAcc, undefined)
            end
    end;

extract_line(Bin, PayloadSize, [{Name, Params, _} | RestMsg], BinHold) ->
    BinSize = byte_size(Bin),
    NextBin = PayloadSize + 2,
    case BinSize >= NextBin of
        true ->
            Payload = binary:part(Bin, {0, PayloadSize}),
            Msg = {Name, Params, Payload},
            NewBin = binary:part(Bin, {NextBin, BinSize - NextBin}),
            extract_line(NewBin, undefined, [Msg | RestMsg], <<>>);
        false ->
            % Input ended without the payload of the last msg completed.
            % Remove that message from the result, and prepend it to the
            % remaining result.
            {RestMsg, <<BinHold/binary, Bin/binary>>}
    end.

extract_msg(Bin, MsgAcc) ->
    {Msg, PayloadSize, Hold} = split_msg(Bin),
    {[Msg | MsgAcc], PayloadSize, Hold}.


split_msg(Bin) ->
    Ret = case binary:match(Bin, ?SEPLIST) of
        nomatch ->
            make_msg(bin_to_name(Bin), <<>>);
        {Pos, Len} ->
            Name = bin_to_name(binary:part(Bin, {0, Pos})),
            Rest = binary:part(Bin, {Pos + Len, byte_size(Bin) - (Pos + Len)}),
            make_msg(Name, Rest)
    end,
    case Ret of
        {_, _, _} ->
            Ret;
        _ ->
            {Ret, undefined, false}
    end.

make_msg(ping, _) -> ping;
make_msg(pong, _) -> pong;
make_msg(ok, _) -> ok;

make_msg(info, Rest) ->
    {info, jsx:decode(Rest, [return_maps])};

make_msg(connect, Rest) ->
    {connect, jsx:decode(Rest, [return_maps])};

make_msg(err, Rest) ->
    % TODO: handle spaces
    {err, err_to_atom(Rest)};

make_msg(sub, Rest) ->
    Params = case binary:split(Rest, ?SEPLIST, [global, trim_all]) of
        [Subject, Sid] ->
            {Subject, undefined, Sid};
        [Subject, QueueGrp, Sid] ->
            {Subject, QueueGrp, Sid}
    end,
    {sub, Params};

make_msg(pub, Rest) ->
    {Params, PayloadSize} = case binary:split(Rest, ?SEPLIST, [global, trim_all]) of
        [Subject, BinBytes] ->
            PS = binary_to_integer(BinBytes),
            {{Subject, undefined, PS}, PS};
        [Subject, ReplyTo, BinBytes] ->
            PS = binary_to_integer(BinBytes),
            {{Subject, ReplyTo, PS}, PS}
    end,
    {{pub, Params, <<>>}, PayloadSize, true};

make_msg(msg, Rest) ->
    {Params, PayloadSize} = case binary:split(Rest, ?SEPLIST, [global, trim_all]) of
        [Subject, Sid, BinBytes] ->
            PS = binary_to_integer(BinBytes),
            {{Subject, Sid, undefined, PS}, PS};
        [Subject, Sid, ReplyTo, BinBytes] ->
            PS = binary_to_integer(BinBytes),
            {{Subject, Sid, ReplyTo, PS}, PS}
    end,
    {{msg, Params, <<>>}, PayloadSize, true};

make_msg(unsub, Rest) ->
    Params = case binary:split(Rest, ?SEPLIST, [global, trim_all]) of
        [Subject] ->
            {Subject, undefined};
        [Subject, BinMaxMsg] ->
            {Subject, binary_to_integer(BinMaxMsg)}
    end,
    {unsub, Params}.


bin_to_name(<<"INFO">>) -> info;
bin_to_name(<<"CONNECT">>) -> connect;
bin_to_name(<<"PUB">>) -> pub;
bin_to_name(<<"SUB">>) -> sub;
bin_to_name(<<"UNSUB">>) -> unsub;
bin_to_name(<<"MSG">>) -> msg;
bin_to_name(<<"PING">>) -> ping;
bin_to_name(<<"PONG">>) -> pong;
bin_to_name(<<"+OK">>) -> ok;
bin_to_name(<<"-ERR">>) -> err.

err_to_atom(<<"'Unknown Protocol Operation'">>) -> unknown_protocol;
err_to_atom(<<"'Authorization Violation'">>) -> auth_violation;
err_to_atom(<<"'Authorization Timeout'">>) -> auth_timeout;
err_to_atom(<<"'Parser Error'">>) -> parser_error;
err_to_atom(<<"'Stale Connection'">>) -> stale_connection;
err_to_atom(<<"'Slow Consumer'">>) -> slow_consumer;
err_to_atom(<<"'Maximum Payload Exceeded'">>) -> max_payload;
err_to_atom(<<"'Invalid Subject'">>) -> invalid_subject;
err_to_atom(_) -> unknown_error.

%% == Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% == Encode Tests

ping_test() ->
    R = ping(),
    E = <<"PING\r\n">>,
    ?assertEqual(E, R).

pong_test() ->
    R = pong(),
    E = <<"PONG\r\n">>,
    ?assertEqual(E, R).

ok_test() ->
    R = ok(),
    E = <<"+OK\r\n">>,
    ?assertEqual(E, R).

err_test() ->
    R = err(auth_timeout),
    E = <<"-ERR 'Authorization Timeout'\r\n">>,
    ?assertEqual(E, R).

info_test() ->
    R = info(#{<<"server_id">> => <<"0001-SERVER">>, <<"auth_required">> => true}),
    E = <<"INFO {\"auth_required\":true,\"server_id\":\"0001-SERVER\"}\r\n">>,
    ?assertEqual(E, R).

connect_test() ->
    R = connect(#{<<"verbose">> => true, <<"name">> => <<"sample-client">>}),
    E = <<"CONNECT {\"name\":\"sample-client\",\"verbose\":true}\r\n">>,
    ?assertEqual(E, R).

pub_1_test() ->
    R = pub(<<"NOTIFY">>),
    E = <<"PUB NOTIFY 0\r\n\r\n">>,
    ?assertEqual(E, R).

pub_3_test() ->
    R = pub(<<"FRONT.DOOR">>, <<"INBOX.22">>, <<"Knock Knock">>),
    E = <<"PUB FRONT.DOOR INBOX.22 11\r\nKnock Knock\r\n">>,
    ?assertEqual(E, R).

sub_2_test() ->
    R = sub(<<"FOO">>, <<"1">>),
    E = <<"SUB FOO 1\r\n">>,
    ?assertEqual(E, R).

sub_3_test() ->
    R = sub(<<"BAR">>, <<"G1">>, <<"44">>),
    E = <<"SUB BAR G1 44\r\n">>,
    ?assertEqual(E, R).

unsub_1_test() ->
    R = unsub(<<"1">>),
    E = <<"UNSUB 1\r\n">>,
    ?assertEqual(E, R).

unsub_2_test() ->
    R = unsub(<<"1">>, 10),
    E = <<"UNSUB 1 10\r\n">>,
    ?assertEqual(E, R).

msg_4_test() ->
    R = msg(<<"FOO.BAR">>, <<"9">>, <<"INBOX.34">>, <<"Hello, World!">>),
    E = <<"MSG FOO.BAR 9 INBOX.34 13\r\nHello, World!\r\n">>,
    ?assertEqual(E, R).

%% == Decode Tests

dec_ping_test() ->
    {[R], _} = decode(<<"PING\r\n">>),
    ?assertEqual(ping, R).

dec_ping_spaces_test() ->
    {[R], _} = decode(<<"PING     \t    \r\n">>),
    ?assertEqual(ping, R).

dec_pong_test() ->
    {[R], _} = decode(<<"PONG\r\n">>),
    ?assertEqual(pong, R).

dec_ok_test() ->
    {[R], _} = decode(<<"+OK\r\n">>),
    ?assertEqual(ok, R).

dec_err_test() ->
    {[R], _} = decode(<<"-ERR 'Authorization Timeout'\r\n">>),
    E = {err, auth_timeout},
    ?assertEqual(E, R).

dec_info_test() ->
    {[R], _} = decode(<<"INFO {\"auth_required\":true,\"server_id\":\"0001-SERVER\"}\r\n">>),
    E = {info,#{<<"auth_required">> => true,
                <<"server_id">> => <<"0001-SERVER">>}},
    ?assertEqual(E, R).

dec_connect_test() ->
    {[R], _} = decode(<<"CONNECT {\"name\":\"sample-client\",\"verbose\":true}\r\n">>),
    E = {connect, #{<<"verbose">> => true, <<"name">> => <<"sample-client">>}},
    ?assertEqual(E, R).

dec_pub_1_test() ->
    {[R], _} = decode(<<"PUB NOTIFY 0\r\n\r\n">>),
    E = {pub, {<<"NOTIFY">>, undefined, 0}, <<>>},
    ?assertEqual(E, R).

dec_pub_2_test() ->
    {[R], _} = decode(<<"PUB FOO 11\r\nHello NATS!\r\n">>),
    E = {pub, {<<"FOO">>, undefined, 11}, <<"Hello NATS!">>},
    ?assertEqual(E, R).

dec_pub_3_test() ->
    {[R], _} = decode(<<"PUB FRONT.DOOR INBOX.22 11\r\nKnock Knock\r\n">>),
    E = {pub, {<<"FRONT.DOOR">>, <<"INBOX.22">>, 11}, <<"Knock Knock">>},
    ?assertEqual(E, R).

dec_sub_2_test() ->
    {[R], _} = decode(<<"SUB FOO 1\r\n">>),
    E = {sub, {<<"FOO">>, undefined, <<"1">>}},
    ?assertEqual(E, R).

dec_sub_3_test() ->
    {[R], _} = decode(<<"SUB BAR G1 44\r\n">>),
    E = {sub,{<<"BAR">>,<<"G1">>,<<"44">>}},
    ?assertEqual(E, R).

dec_unsub_1_test() ->
    {[R], _} = decode(<<"UNSUB 1\r\n">>),
    E = {unsub, {<<"1">>, undefined}},
    ?assertEqual(E, R).

dec_unsub_2_test() ->
    {[R], _} = decode(<<"UNSUB 1 10\r\n">>),
    E = {unsub, {<<"1">>, 10}},
    ?assertEqual(E, R).

dec_msg_3_test() ->
    {[R], _} = decode(<<"MSG FOO.BAR 9 13\r\nHello, World!\r\n">>),
    E = {msg, {<<"FOO.BAR">>, <<"9">>, undefined, 13}, <<"Hello, World!">>},
    ?assertEqual(E, R).

dec_msg_4_test() ->
    {[R], _} = decode(<<"MSG FOO.BAR 9 INBOX.34 13\r\nHello, World!\r\n">>),
    E = {msg, {<<"FOO.BAR">>, <<"9">>, <<"INBOX.34">>, 13}, <<"Hello, World!">>},
    ?assertEqual(E, R).

dec_many_lines_test() ->
    {[R1, R2], _} = decode(<<"PING\r\nMSG FOO.BAR 9 INBOX.34 13\r\nHello, World!\r\n">>),
    E1 = ping,
    ?assertEqual(E1, R1),
    E2 = {msg, {<<"FOO.BAR">>, <<"9">>, <<"INBOX.34">>, 13}, <<"Hello, World!">>},
    ?assertEqual(E2, R2).

dec_remaining_test() ->
    B = <<"SUB DEVICE.cikbsij2b0000m37h05a7m5oe.OUT 2\r\nSUB DEVICE.cikbsij2d0001m37h2u271jic.OUT 3\r\nSUB DEVICE.cikbsij2e0002m3">>,
    {Msgs, Rem} = decode(B),
    ?assertEqual(2, length(Msgs)),
    ?assertEqual(<<"SUB DEVICE.cikbsij2e0002m3">>, Rem).

dec_nl_in_payload_test() ->
    B = <<"PUB FOO 12\r\nHello\r\nNATS!\r\n">>,
    {[M], Rem} = decode(B),
    E = {pub, {<<"FOO">>, undefined, 12}, <<"Hello\r\nNATS!">>},
    ?assertEqual(E, M),
    ?assertEqual(Rem, <<>>).

decl_incomplete_payload_test() ->
    B = <<"SUB BAR 3\r\nPUB FOO 12\r\nHello\r\nNATS!">>,
    {Msgs, Rem} = decode(B),
    ?assertEqual(1, length(Msgs)),
    E1 = {sub, {<<"BAR">>, undefined, <<"3">>}},
    ?assertEqual(E1, hd(Msgs)),
    E2 = <<"PUB FOO 12\r\nHello\r\nNATS!">>,
    ?assertEqual(E2, Rem).



% == Other Tests

decode_encode_1_test() ->
    E = <<"MSG FOO.BAR 9 INBOX.34 13\r\nHello, World!\r\n">>,
    {[R1], _} = decode(E),
    io:format("R1: ~p~n", [R1]),
    R2 = encode(R1),
    ?assertEqual(E, R2).

-endif.
