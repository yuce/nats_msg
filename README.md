# nats_msg


**nats_msg** is a pure Erlang NATS message encoder/decoder library for
[NATS](http://nats.io/) high performance messaging platform.
For details about NATS protocol, see:
[NATS Protocol](http://nats.io/documentation/internals/nats-protocol). It doesn't
have any dependency other than Erlang/OTP (16+ *should* be OK) and optionally
[rebar3](http://www.rebar3.org/).

## News

* **Version 0.4.0** (*2016-03-23*):
    * This version is incompatible with previous versions of the library
    * Decoding is lazy, in the sense that, only a single message is decoded whenever
    the decode is called
    * HUGE performance improvements (2x - 100x)
    * The parser is much more stricter now
    * Encoding functions return iodata instead of binary
    * You can use iodata anywhere a binary is expected


## Install

**nats_msg** uses [rebar3](http://www.rebar3.org/) to build and tests and
it is available on [hex.pm](https://hex.pm/). Just include the following
in your `rebar.config`:

```erlang
{deps, [nats_msg]}.
```

## Tests

Run the tests using:

    $ rebar3 eunit

## Build

    $ rebar3 compile

## Usage

> IMPORTANT!
>
> Before running any decoding functions, `nats_msg:init/0` must be called once.

Binaries and iodata are used exclusively throughout the library.

Currently, no error handling is performed during encoding/decoding. You can protect
against crashes by wrapping library functions between `try...catch`.

`INFO` and `CONNECT` messages have a JSON object as their parameter; but in order to
not introduce a dependency, **nats_msg** does not encode/decode JSON objects. These parameters
are kept or returned as binaries. You can use [jsx](https://github.com/talentdeficit/jsx) or [jiffy](https://github.com/davisp/jiffy)
to deal with JSON. See the **INFO** and **CONNECT** sections in this document for examples.

### Encoding

Encoding a message produces an IO list. `nats_msg:encode/1` takes an atom as the name of the
message or a tuple which contains the name, parameters and payload of the message.

The general form of `nats_msg:encode/1` parameters is:

* `Name :: atom()`: For messages taking no parameters,
* `{Name :: atom(), Parameters :: {iodata() | int(), ...}}` for messages taking parameters but
not a payload,
* `{Name :: atom(), Parameters :: {iodata() | int(), ...}, Payload :: iodata()}` for messages
taking parameters and a payload.

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

Decoding a binary/iodata produces a `{Message, RemainingBinary}` tuple.
`Message` is either `[]` if the data is not sufficient to decode the message, or the a term for the message,
and `RemainingBinary` is the part of the input which
wasn't decoded and returned. The latter is very useful when dealing with streams, where
the input is chunked and appending chunks is required to be able to decode messages.
In those situations, just prepend `RemainingBinary` to the next binary chunk before attempting
to decode it.

The `Message`  be used as an input to `nats_msg:encode`, like:

```erlang
SomeBinary = ...
{Msg, Remaining} = nats_msg:decode(SomeBinary),
ReEncodedBinary = nats_msg:encode(Msg),
% ReEncodedBinary = SomeBinary
```

### INFO Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#INFO)

#### Encode

```erlang
ServerInfo = #{<<"auth_required">> => true, <<"server_id">> => <<"0001-SERVER">>},
BinaryInfo = jsx:encode(ServerInfo),
BinaryMsg = nats_msg:info(BinaryInfo).
```

#### Decode

```erlang
Chunk = <<"INFO {\"auth_required\":true,\"server_id\":\"0001-SERVER\"}\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{info, BinaryInfo} = Msg,
ServerInfo = jsx:decode(BinaryInfo, [return_maps]).
```

### CONNECT Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#CONNECT)

#### Encode

```erlang
ConnectInfo = #{<<"auth_required">> => true, <<"server_id">> => <<"0001-SERVER">>},
BinaryInfo = jsx:encode(ServerInfo),
BinaryMsg = nats_msg:connect(BinaryInfo).
```

#### Decode

```erlang
Chunk = <<"CONNECT {\"verbose\":true,\"name\":\"the_client\"}\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{connect, BinaryInfo} = Msg,
ClientInfo = jsx:decode(BinaryInfo, [return_maps]).
```

### PUB Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#PUB)

#### Encode

Notify subscribers of a subject:

```erlang
BinaryMsg = nats_msg:pub(<<"NOTIFY.INBOX">>).
```

Send some data (*payload*) to subscribers, providing a *reply* subject:

```erlang
BinaryMsg = nats_msg:pub(<<"FOOBAR">>, <<"REPRAP">>, <<"Hello, World!">>).
```

Send some data (*payload*) to subscribers (*without a reply subject*):

```erlang
BinaryMsg = nats_msg:pub(<<"FOOBAR">>, undefined, <<"Hello, World!">>).
```

### Decode

Publish notification:

```erlang
Chunk = <<"PUB NOTIFY 0\r\n\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{pub, {Subject, ReplyTo, Payload}} = Msg,
% Subject = <<"NOTIFY">>,
% ReplyTo = undefined,
% Payload = <<>>.
```

Publish message with subject, replier and payload:

```erlang
Chunk = <<"PUB FRONT.DOOR INBOX.22 11\r\nKnock Knock\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{pub, {Subject, ReplyTo, Payload}} = Msg,
% Subject = <<"FRONT.DOOR">>,
% ReplyTo = <<"INBOX.22">>,
% Payload = <<"Knock Knock">>.
```

### SUB Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#SUB)

#### Encode

Subscribe message with subject and SID:

```erlang
BinaryMsg = nats_msg:sub(<<"FOO">>, <<"1">>).
```

Subscribe message with subject, group queue and SID:

```erlang
BinaryMsg = nats_msg:sub(<<"BAR">>, <<"G1">>, <<"44">>)
```

#### Decode

```erlang
Chunk = <<"SUB FOO 1\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{sub, {Subject, GroupQueue, Sid}} = Msg,
% Subject = <<"FOO">>,
% GroupQueue = undefined,
% Sid = <<"1">>.
```

### UNSUB Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#UNSUB)

#### Encode

Unsubscribe message with SID:

```erlang
BinaryMsg = nats_msg:unsub(<<"1">>).
```

Unsubscribe message with SID and *max messages*:

```erlang
BinaryMsg = nats_msg:unsub(<<"1">>, 10).
```

#### Decode

```erlang
Chunk = <<"UNSUB 1 10\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{unsub, {Sid, MaxMessages}} = Msg,
% Sid = <<"1">>,
% MaxMessages = 10
```

### MSG Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#MSG)

#### Encode

Message with subject and SID:

```erlang
BinaryMsg = nats_msg:msg(<<"FOO">>, <<"5">>).
```

Message with subject, sid, *reply to subject* and payload:

```erlang
BinaryMsg = nats_msg:msg(<<"FOO">>, <<"5">>, <<"INBOX">>, <<"Hello!">>).
```

Message with subject, sid and payload:

```erlang
BinaryMsg = nats_msg:msg(<<"FOO">>, <<"5">>, undefined, <<"Hello!">>).
```

#### Decode

Message with subject, sid and payload:

```erlang
Chunk = <<"MSG FOO.BAR 9 13\r\nHello, World!\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{msg, {Subject, Sid, ReplyTo, Payload}} = Msg,
% Subject = <<"FOO.BAR">>,
% Sid = <<"9">>,
% ReplyTo = undefined,
% Payload = <<"Hello, World!">>.
```

### PING Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#PING)

#### Encode

```erlang
BinaryMsg = nats_msg:ping().
```

#### Decode

```erlang
{Msg, _} = nats_msg:decode(<<"PING\r\n">>),
% Msg = ping
```

### PONG Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#PONG)

#### Encode

```erlang
BinaryMsg = nats_msg:pong().
```

#### Decode

```erlang
{Msg, _} = nats_msg:decode(<<"PONG\r\n">>),
% Msg = pong
```

### +OK Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#OKERR)

#### Encode

```erlang
BinaryMsg = nats_msg:ok().
```

#### Decode

```erlang
{Msg, _} = nats_msg:decode(<<"+OK\r\n">>),
% Msg = ok
```

### -ERR Message

[NATS Spec](http://nats.io/documentation/internals/nats-protocol/#OKERR)

The spec defines a predefined set of error messages, so **nats_msg** encodes/decodes these
to/from atoms as:

* `'Unknown Protocol Operation'` => `unknown_operation`
* `'Authorization Violation'` => `auth_violation`
* `'Authorization Timeout'` => `auth_timeout`
* `'Parser Error'` => `parser_error`
* `'Stale Connection'` => `stale_connection`
* `'Slow Consumer'` => `slow_consumer`
* `'Maximum Payload Exceeded'` => `max_payload`
* `'Invalid Subject'` => `invalid_subject`
* Other errors are converted to `unknown_error` during decoding and kept as is during encoding.

#### Encode

```erlang
BinaryMsg = nats_msg:err(auth_violation).
```

#### Decode

```erlang
Chunk = <<"-ERR 'Authorization Timeout'\r\n">>,
{Msg, _} = nats_msg:decode(Chunk),
{ok, Error} = Msg,
% Error = auth_timeout
```

## License

```
Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

* The names of its contributors may not be used to endorse or promote
  products derived from this software without specific prior written
  permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
