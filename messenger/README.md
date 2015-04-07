Messenger
====
Messenger based on module gen_server on Erlang, Contains a Login Server, Messages are sent in P2P among Clients.

Configuration
====
Please edit the line in client.erl for the server node
```
-define(SERVER, 'server@ubuntu1404d').
```

Run
====
##On Server Node
Start Login Service
```
shell> server:start().
```

##Client
Login
```
shell> client:login("NAME_HERE").
```
Messaging
```
shell> client:send("TONAME","MESSAGE_HERE").
```
