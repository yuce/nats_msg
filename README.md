# nats_msg


**nats_msg** is a pure Erlang NATS message encoder/decoder library for
[NATS](http://nats.io/) high performance messaging platform.
For details about NATS protocol, see:
[NATS Protocol](http://nats.io/documentation/internals/nats-protocol). It doesn't
have any dependency other than Erlang/OTP install (16+ *should* be OK) and [rebar3](http://www.rebar3.org/).

## Install


**nats_msg** uses [rebar3](http://www.rebar3.org/) to build and tests and
it is available on [hex.pm](https://hex.pm/). Just include the following
in your `rebar.config`:

    {deps[nats_msg]}.

Alternatively (*for whatever reason you don't like to use hex.pm*):

    {deps, [
        {nat_msg, {git, "https://github.com/yuce/nats_msg.git", {branch, "master"}}.

Or, you can just copy `src/nats_msg.erl` to your project to use it.

## Tests

Run the tests using:

    $ rebar3 eunit

## Build

    $ rebar3 compile

## Usage

Binaries are used exclusively throughout the library.

Currently, no error handling is performed during encoding/decoding. You can protect
against crashes by wrapping library functions between `try...catch`.

### Encoding

Encoding a message produces a binary. `nats_msg:encode/1` takes an atom as the name of the
message or a tuple which contains the name, parameters and payload of the message.

The general form of `nats_msg:encode/1` parameters is:

* `Name :: atom()`: For messages taking no parameters,
* `{Name :: atom(), Parameters :: {binary() | int(), ...}}` for messages taking parameters but
no payloads,
* `{Name :: atom(), Parameters :: {binary() | int(), ...}, Payload :: binary()}` for messages
taking parameters and payloads.

Messages of the same type always have the same structure, even if some of the values are
`undefined`. Some examples:

* `nats_msg:encode(ping)` produces a `PING` message,
* `nats_msg:encode({sub, {<<"INBOX">>, undefined, <<"2">>}}` produces a `SUB` message with
subject `<<"INBOX">>` and SID `<<"2">>`. This particular message has no *queue group*, so
that field is set to `undefined`.
* `nats_msg:encode({pub, {<<"FOO">>, undefined, 11}, <<"Hello NATS!">>}` produces a `PUB` message
with subject `<<"FOO">>` and payload `<<"Hello NATS!">>` of size `11` and no *reply to* subject.

The library has convenience functions for all messages, like `nats_msg:ping/0`, which are
discussed later in this document.

### Decoding

Decoding a binary produces a `{Messages, RemainingBinary}` tuple.
`Messages` is a list of messages and `RemainingBinary` is the part of the input which
wasn't decoded and returned. The latter is very useful when dealing with streams, where
the input is chunked and requires appending chunks to be able to decode messages.
In those situations, just prepend `RemaningBinary` to the next binary chunk.

Messages in the `Messages` list can be used as inputs to `nats_msg:encode`, like:

    SomeBinary = ...
    {[Msg | _], Remaining} = nats_msg:decode(SomeBinary),
    ReEncodedBinary = nats_msg:encode(Msg),
    % ReEncodedBinary = SomeBinary

### INFO Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#INFO)

#### Encode

This message requires a JSON object but in order to not introduce a dependency, `nats_msg:info/1`
takes a binary. You can use [jsx](https://github.com/talentdeficit/jsx) or [jiffy](https://github.com/davisp/jiffy)
to convert an Erlang map to binary:

    ServerInfo = #{<<"auth_required">> => true, <<"server_id">> => <<"0001-SERVER">>},
    BinaryInfo = jsx:encode(ServerInfo),
    BinaryMsg = nats_msg:info(BinaryInfo).

#### Decode

This message requires a JSON object but in order to not introduce a dependency, `nats_msg:decode/1`
returns `{info, BinaryInfo}` messages. You can use [jsx](https://github.com/talentdeficit/jsx) or [jiffy](https://github.com/davisp/jiffy)
to convert `BinaryInfo` to a map:

    Chunk = <<"INFO {\"auth_required\":true,\"server_id\":\"0001-SERVER\"}\r\n">>,
    {[Msg], _} = nats_msg:decode(Chunk),
    {info, BinaryInfo} = Msg,
    ServerInfo = jsx:decode(BinaryInfo, [return_maps]).

### CONNECT Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#CONNECT)

#### Encode

This message requires a JSON object but in order to not introduce a dependency, `nats_msg:connect/1`
takes a binary. You can use [jsx](https://github.com/talentdeficit/jsx) or [jiffy](https://github.com/davisp/jiffy)
to convert an Erlang map to binary:

    ConnectInfo = #{<<"auth_required">> => true, <<"server_id">> => <<"0001-SERVER">>},
    BinaryInfo = jsx:encode(ServerInfo),
    BinaryMsg = nats_msg:connect(BinaryInfo).

#### Decode

This message requires a JSON object but in order to not introduce a dependency, `nats_msg:decode/1`
returns `{connect, BinaryInfo}` messages. You can use [jsx](https://github.com/talentdeficit/jsx) or [jiffy](https://github.com/davisp/jiffy)
to convert `BinaryInfo` to a map:

    Chunk = <<"CONNECT {\"verbose\":true,\"name\":\"the_client\"}\r\n">>,
    {[Msg], _} = nats_msg:decode(Chunk),
    {connect, BinaryInfo} = Msg,
    ClientInfo = jsx:decode(BinaryInfo, [return_maps]).

### PUB Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#PUB)

Subjects in NATS protocol are case-insensitive, so `<<"foo.bar">>` and `<<"FOO.Bar">>`
are equivalent subjects. `nats_msg` doesn't transform subjects in the encoded/decoded message,
so it's up to the application to deal with this detail.

#### Encode

Notify subscribers of a subject:

    BinaryMsg = nats_msg:pub(<<"NOTIFY.INBOX">>).

Send some data (*payload*) to subscribers, providing a *reply* subject:

    BinaryMsg = nats_msg:pub(<<"FOOBAR">>, <<"REPRAP">>, <<"Hello, World!">>).

Send some data (*payload*) to subscribers (*without a reply subject*):

    BinaryMsg = nats_msg:pub(<<"FOOBAR">>, undefined, <<"Hello, World!">>).

### Decode

Publish notification:

    Chunk = <<"PUB NOTIFY 0\r\n\r\n">>,
    {[Msg], _} = nats_msg:decode(Chunk),
    {pub, {Subject, ReplyTo, PayloadSize}, Payload} = Msg,
    % Subject = <<"NOTIFY">>,
    % ReplyTo = undefined,
    % PayloadSize = 0,
    % Payload = <<>>.

Publish message with replier and payload:

    Chunk = <<"PUB FRONT.DOOR INBOX.22 11\r\nKnock Knock\r\n">>,
    {[Msg], _} = nats_msg:decode(Chunk),
    {pub, {Subject, ReplyTo, PayloadSize}, Payload} = Msg,
    % Subject = <<"FRONT.DOOR">>,
    % ReplyTo = <<"INBOX.22">>,
    % PayloadSize = 11,
    % Payload = <<"Knock Knock">>.

### SUB Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#SUB)

Subjects in NATS protocol are case-insensitive, so `<<"foo.bar">>` and `<<"FOO.Bar">>`
are equivalent subjects. `nats_msg` doesn't transform subjects in the encoded/decoded message,
so it's up to the application to deal with this detail.

### UNSUB Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#UNSUB)

### MSG Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#MSG)

Subjects in NATS protocol are case-insensitive, so `<<"foo.bar">>` and `<<"FOO.Bar">>`
are equivalent subjects. `nats_msg` doesn't transform subjects in the encoded/decoded message,
so it's up to the application to deal with this detail.


### PING Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#PING)

### PONG Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#PONG)

### +OK Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#OKERR)

### -ERR Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#OKERR)