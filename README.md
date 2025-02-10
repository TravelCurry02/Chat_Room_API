# Chat Room API

## Overview

This project is a **scalable chat room system** implemented in **Erlang**. It showcases the power of concurrent programming by utilizing Erlang's lightweight processes, supervision trees, and message-passing mechanisms to create a **robust, distributed chat application**.

### Key Features:
- **Modular Design** – Separate modules for supervision, pub-sub messaging, and transport.
- **Real-time Communication** – Supports multiple concurrent users with minimal latency.
- **Fault-Tolerant** – Uses OTP's **gen_server** and **supervisors** to handle failures gracefully.
- **Scalable Architecture** – Horizontally scalable for real-world applications.

---

## Development Environment

- **Programming Language:** Erlang
- **Build Tool:** Rebar3
- **Tools Used:**
  - Makefile for automated builds
  - Supervisors for fault tolerance
  - Pub-Sub modules for efficient message broadcasting
  - Distributed messaging architecture for scalability

---

## Project Architecture

- **Chat Room Management:** Handles creating, deleting, and listing available chat rooms.
- **Message Handling:** Ensures non-blocking sending and receiving of messages.
- **Pub-Sub Module:** Enables users to subscribe and receive messages in real time.
- **Supervision Tree:** Restarts failed processes automatically for resilience.
- **Transport Layer:** Manages serialization and deserialization of messages for future protocol support.

---

## Scalability

This chat system is designed to support **high concurrency and multiple chat rooms** efficiently. Scalability is achieved through:

- **Lightweight Processes** – Erlang allows handling thousands of users with minimal overhead.
- **Supervision Trees** – Fault tolerance ensures minimal downtime and auto-recovery.
- **Distributed Architecture** – The system can be deployed across multiple nodes for horizontal scaling.
- **Efficient Message Passing** – Erlang’s built-in messaging system enables quick and reliable communication.
- **Load Balancing** – Chat rooms can be distributed across multiple nodes to prevent performance bottlenecks.

---

## How to Use

### Installation

#### Erlang
In your `rebar.config`:

```erlang
{deps, [
  {chat_room, "0.3.0", {pkg, chat_room}}
]}.
```


### Getting Started
Assuming you have a working Erlang installation (18 or later), building chat_room should be as simple as:

```sh
$ git clone https://github.com/TravelCurry02/Chat_Room_API
$ cd chat-room-api
$ make
```

### Quick Start Example
Start an Erlang console with chat_room running:

```sh
make shell
```

Once in the Erlang console:

```erlang
% Subscribe the current shell process to "general_chat"
chat_room:sub(self(), "general_chat").
ok

% Spawn a new process to represent another user
Pid = spawn_link(fun() -> timer:sleep(infinity) end).
<0.169.0>

% Subscribe the new user process to "general_chat"
chat_room:sub(Pid, "general_chat").
ok

% Publish a message to "general_chat"
chat_room:pub("general_chat", {general_chat, "Hello, everyone!"}).
ok

% Check received messages for the new user
chat_room_proc:messages(Pid).
[{message,"Hello, everyone!"}]

% Check received messages for self
chat_room_proc:messages(self()).
[{message,"Hello, everyone!"}]

% Unsubscribe self from "general_chat"
chat_room:unsub(self(), "general_chat").
ok

% Publish another message
chat_room:pub("general_chat", {general_chat, "How is everyone?"}).
ok

% Check received messages for the new user
chat_room_proc:messages(Pid).
[{general_chat,"Hello, everyone!"},{general_chat,"How is everyone?"}]

% Check received messages for self (last message didn't arrive)
chat_room_proc:messages(self()).
[{general_chat,"Hello, everyone!"}]

% Check subscribers for "general_chat" (only Pid should be in the returned list)
chat_room:subscribers("general_chat").
[<0.169.0>]

% List all active chat rooms
chat_room:topics().
[<<"general_chat">>]

% Subscribe self to "tech_discussion"
chat_room:sub(self(), "tech_discussion").
ok

% List all active chat rooms
chat_room:topics().
[<<"tech_discussion">>,<<"general_chat">>]

% Publish a message in "tech_discussion"
chat_room:pub("tech_discussion", {tech_discussion, "Erlang is great!"}).
ok

% Check received messages for the new user (last message didn't arrive)
chat_room_proc:messages(Pid).
[{general_chat,"Hello, everyone!"},{general_chat,"How is everyone?"}]

% Check received messages for self
chat_room_proc:messages(self()).
[{general_chat,"Hello, everyone!"},{tech_discussion,"Erlang is great!"}]
```

---

## Useful Websites

- [Erlang Documentation](https://www.erlang.org/docs)  
- [Rebar3 Documentation](https://rebar3.org/)  
- [Erlang Process Model](https://learnyousomeerlang.com/)  
- [Erlang OTP Behaviors](https://erlang.org/doc/design_principles/des_princ.html)  

---

## Future Work

- Implement **user authentication** for better security
- Add **persistent message storage** with a database backend
- Extend support for **real-time WebSocket communication**
- Enhance scalability with **distributed node clustering**
- Implement **end-to-end encryption** for secure communication

**This project is a foundation for building scalable and resilient chat applications in Erlang!**

