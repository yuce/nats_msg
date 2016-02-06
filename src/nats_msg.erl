-module('nats_msg').

%% API exports
-export([encode/1,
         decode/1,
         info/1,
         connect/1,
         pub/1,
         pub/2,
         pub/3,
         sub/2,
         sub/3,
         msg/3,
         msg/4,
         unsub/1,
         unsub/2,
         ping/0,
         pong/0,
         ok/0,
         err/1]).

-define(SEP, <<" ">>).
-define(NL, <<"\r\n">>).

%% == API functions

-spec info(Info :: map()) -> binary().
info(Info) ->
    BinInfo = jsx:encode(Info),
    encode({info, [BinInfo], undefined}).

-spec connect(Info :: map()) -> binary().
connect(Info) ->
    BinInfo = jsx:encode(Info),
    encode({connect, [BinInfo], undefined}).

-spec pub(Subject :: binary(), ReplyTo :: binary(), Payload :: binary()) ->
    binary().
pub(Subject, ReplyTo, Payload) ->
    Params = case ReplyTo of
        <<>> -> [Subject, integer_to_binary(byte_size(Payload))];
        _ -> [Subject, ReplyTo, integer_to_binary(byte_size(Payload))]
    end,
    encode({pub, Params, Payload}).

pub(Subject) ->
    pub(Subject, <<>>).

pub(Subject, Payload) ->
    pub(Subject, <<>>, Payload).

-spec sub(Subject :: binary(), QueueGrp :: binary(), Sid :: binary()) ->
    binary().
sub(Subject, QueueGrp, Sid) ->
    Params = case QueueGrp of
        <<>> ->
            [Subject, Sid];
        _ ->
            [Subject, QueueGrp, Sid]
    end,
    encode({sub, Params, undefined}).

sub(Subject, Sid) ->
    sub(Subject, <<>>, Sid).

-spec unsub(Subject :: binary(), MaxMsg :: integer()) ->
    binary().
unsub(Subject, MaxMsg) ->
    Params = case MaxMsg of
        0 ->
            [Subject];
        M when M > 0 ->
            [Subject, integer_to_binary(M)]
    end,
    encode({unsub, Params, undefined}).

unsub(Subject) ->
    unsub(Subject, 0).

-spec msg(Subject :: binary(), Sid :: binary(), ReplyTo :: binary(), Payload :: binary()) ->
    binary().
msg(Subject, Sid, ReplyTo, Payload) ->
    Params = case ReplyTo of
        <<>> -> [Subject, Sid, integer_to_binary(byte_size(Payload))];
        _ -> [Subject, Sid, ReplyTo, integer_to_binary(byte_size(Payload))]
    end,
    encode({msg, Params, Payload}).    

msg(Subject, Sid, Payload) ->
    msg(Subject, Sid, <<>>, Payload).

ping() -> encode({ping, [], undefined}).
pong() -> encode({pong, [], undefined}).
ok() -> encode({ok, [], undefined}).

-spec err(ErrMsg :: binary()) -> binary().

err(unknown_protocol) ->
    err(<<"'Unknown Protocol Operation'">>);

err(auth_violation) ->
    err(<<"'Authorization Violation'">>);

err(auth_timeout) ->
    err(<<"'Authorization Timeout'">>);

err(parser_error) ->
    err(<<"'Parser Error'">>);

err(stale_connection) ->
    err(<<"'Stale Connection'">>);

err(slow_consumer) ->
    err(<<"'Slow Consumer'">>);

err(max_payload) ->
    err(<<"'Maximum Payload Exceeded'">>);

err(ErrMsg) ->
    encode({err, [ErrMsg], undefined}).

-spec encode({Name :: atom(),  Params :: [binary()], Payload :: binary()}) ->
    Message :: binary().

encode({Name, Params, Payload}) ->
    Encoded = encode_message(Name, Params, Payload),
    iolist_to_binary(Encoded).

-spec decode(Message :: binary()) ->
    [{Name ::atom(), Params :: [binary()], Payload :: binary()}].
decode(Message) ->
    [].

%% == Internal functions

name_to_binary(info) -> <<"INFO">>;
name_to_binary(connect) -> <<"CONNECT">>;
name_to_binary(pub) -> <<"PUB">>;
name_to_binary(sub) -> <<"SUB">>;
name_to_binary(unsub) -> <<"UNSUB">>;
name_to_binary(msg) -> <<"MSG">>;
name_to_binary(ping) -> <<"PING">>;
name_to_binary(pong) -> <<"PONG">>;
name_to_binary(ok) -> <<"+OK">>;
name_to_binary(err) -> <<"-ERR">>.

encode_message(Name, Params, Payload) ->
    R1 = [name_to_binary(Name)],
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



% interp_data(Data, State) ->
%     F = fun(Msg, {ResponseAcc, StateAcc}) ->
%         case interp_msg(Msg, StateAcc) of
%             {<<>>, NewState} ->
%                 {ResponseAcc, NewState};
%             {Response, NewState} ->
%                 {[Response | ResponseAcc], NewState}
%         end
%     end,
%     Messages = binary:split(Data, <<"\r\n">>, [global, trim_all]),
%     {Response, NewState} = lists:foldl(F, {[], State}, Messages),
%     {lists:reverse(Response), NewState}.

% interp_msg(Msg, State) ->
%     {Name, Payload} = case binary:match(Msg, [<<" ">>, <<"\t">>]) of
%         nomatch ->
%             {Msg, <<>>};
%         PosLen ->
%             N = msg_name(Msg, PosLen),
%             P = msg_payload(Msg, PosLen, byte_size(Msg)),
%             {N, P}
%     end,
%     interp({Name, Payload}, State).

% msg_name(Msg, {Pos, _Len}) ->
%     binary:part(Msg, {0, Pos}).

% msg_payload(Msg, {Pos, Len}, Size) ->
%     binary:part(Msg, {Pos + Len, Size - Pos - Len}).

% interp([], {Response, State}) ->
%     {Response, State};

% interp({<<"CONNECT">>, Payload}, State) ->
%     ClientInfo = jsx:decode(Payload, [return_maps]),
%     NewState = State#{client_info => ClientInfo},
%     Response = case maps:get(verbose, ClientInfo, false) of
%         true -> <<"+OK\r\n">>;
%         false -> <<>>
%     end,
%     {Response, NewState};

% interp({<<"PING">>, _}, State) ->
%     {<<"PONG\r\n">>, State};

% interp({<<"SUB">>, Payload}, #{service := Service} = State) ->
%     io:format("State: ~p~n", [State]),
%     [Subject, Sid] = binary:split(Payload,
%                                   [<<" ">>, <<"\t">>],
%                                   [global, trim_all]),
%     io:format("Sub:~s sid:~s~n", [Subject, Sid]),
%     gnatsd_service:subscribe(Service, Subject, self()),
%     {<<>>, State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
    R = info(#{server_id => <<"0001-SERVER">>, auth_required => true}),
    E = <<"INFO {\"auth_required\":true,\"server_id\":\"0001-SERVER\"}\r\n">>,
    ?assertEqual(E, R).

connect_test() ->
    R = connect(#{verbose => true, name => <<"sample-client">>}),
    E = <<"CONNECT {\"name\":\"sample-client\",\"verbose\":true}\r\n">>,
    ?assertEqual(E, R).

pub_1_test() ->
    R = pub(<<"NOTIFY">>),
    E = <<"PUB NOTIFY 0\r\n\r\n">>,
    ?assertEqual(E, R).

pub_2_test() ->
    R = pub(<<"FOO">>, <<"Hello NATS!">>),
    E = <<"PUB FOO 11\r\nHello NATS!\r\n">>,
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

msg_3_test() ->
    R = msg(<<"FOO.BAR">>, <<"9">>, <<"Hello, World!">>),
    E = <<"MSG FOO.BAR 9 13\r\nHello, World!\r\n">>,
    ?assertEqual(E, R).

msg_4_test() ->
    R = msg(<<"FOO.BAR">>, <<"9">>, <<"INBOX.34">>, <<"Hello, World!">>),
    E = <<"MSG FOO.BAR 9 INBOX.34 13\r\nHello, World!\r\n">>,
    ?assertEqual(E, R).


-endif.