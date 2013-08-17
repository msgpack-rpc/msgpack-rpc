# MessagePack-RPC Specification

## Notice

This page describes the specification of MessagePack-RPC Protocol, the Remote Procedure Call (RPC) Protocol using MessagePack data format. This information is required for developing the MessagePack-RPC language bindings.

MessagePack-RPC enables the client to call pre-defined server functions remotely. The merit of MessagePack-RPC is as follows.

## Compact

The message between the clients and the server is packed as MessagePack data format. It's really compact compared to other format like JSON, XML, etc. The network bandwidth can be reduced dramatically.

## Fast

The implementation of MessagePack-RPC  is really fast by careful design for the modern hardware (multi-core, multi-cpu, etc). The stream deserialization + zero-copy feature effectively overlaps the network transfer and the computation (e.g. deserialization).

## Packaged

The language bindings of MessagePack-RPC is well packaged, by using the default packaging system for each language (e.g. gem for Ruby).

## Rich

Some client implementation supports asynchronous calls. The user is able to overlap the multiple RPC calls in parallel.

# MessagePack-RPC Protocol specification

The protocol consists of "Request" message and the corresponding "Response" message. The server must send "Response" message in reply with the "Request" message.

## Request Message

The request message is a four elements array shown below, packed by MessagePack format.

{code}
[type, msgid, method, params]
{code}

### type

Must be zero (integer). Zero means that this message is the "Request" message.

### msgid

The 32-bit unsigned integer number. This number is used as a sequence number. The server replies with a requested msgid.

### method

The string, which represents the method name.

### params

The array of the function arguments. The elements of this array is arbitrary object.

## Response Message

The response message is a four elements array shown below, packed by MessagePack format.

{code}
[type, msgid, error, result]
{code}

### type

Must be one (integer). One means that this message is the "Response" message.

### msgid

The 32-bit unsigned integer number. This corresponds to the request message.

### error

If the method is executed correctly, this field is Nil. If the error occurred at the server-side, then this field is an arbitrary object which represents the error.

### result

An arbitrary object, which represents the returned result of the function. If error occurred, this field should be nil.

## Notification Message

The notification message is a three elements array shown below, packed by MessagePack format.

{code}
[type, method, params]
{code}

### type

Must be two (integer). Two means that this message is the "Notification" message.

### method

The string, which represents the method name.

### params

The array of the function arguments. The elements of this array is arbitrary object.

# The Order of the Response

The server implementations don't need to send the reply, in the order of the received requests. If they receive the multiple messages, they can reply in random order.

This is required for the pipelining. At the server side, some functions are fast, and some are not. If the server must reply with in order, the slow functions delay the other replies even if it's execution is already completed.

!feature-pipeline.png|border=1!

# Client Implementation Details

There're some client features which client library should implement.

## Step 1: Synchronous Call

The first step is to implement the synchronous call. The client is blocked until the RPC is finished.

{code}
Client client = new Client("localhost", 1985);
Object result = client.call("method_name", arg1, arg2, arg3);
{code}

## Step 2: Asynchronous Call

The second step is to support the asynchronous call. The following figure shows how asynchronous call works.

!feature-async.png|border=1!

The call function finished immediately, and returns the Future object. Then, the user waits the completion of the call by calling join() function. Finally, it gets the results by calling getResult() function.

{code}
Client client = new Client("localhost", 1985);
Future future = client.asyncCall("method_name", arg1, arg2, arg3);
future.join();
Object result = future.getResult();
{code}

This feature is useful when you call multiple functions at the same time. The example code below overlaps the two resquests, by using async calls.

{code}
Client client = new Client(...);
Future f1 = client.asyncCall("method1");
Future f2 = client.asyncCall("method2");
f1.join();
f2.join();
{code}

Implementing the asynchronous call may require the event loop library. Currently, following libraries are used.

* C++: [mpio|http://github.com/frsyuki/mpio]
* Ruby: [Rev|http://rev.rubyforge.org/rdoc/]
* Java: [JBoss netty|http://www.jboss.org/netty]

## Step 3: Multiple Transports

The implementation should support multiple transports like TCP, UDP, UNIX domain socket if possible.

# Server Implementation Details

There're many choices on server architectures (e.g. single-threaded, event-based, multi-threaded, SEDA, etc). If depends on the library implementation.