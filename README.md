erlang web framework
===================
[Change Log](CHANGELOG.md) - What's changed?

**Announcement**: Please note that erlang OTP Version must be (17.0+)

> <sup>\*</sup> mongodb, redis, mysql client will be integrated into the framework.


Quick Start
------------

Use the devtools to get started quickly.

## Dependences

### MacOS

```
$ brew upgrade && brew install erlang

$ erl
Erlang/OTP 18 [erts-7.0.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.0.3  (abort with ^G)
1>

$ mkdir proj && cd proj
$ git clone git@github.com:robertzhouxh/erlangWebMonitor.git
$ cd erlangWebMonitor
$ make all && make rel
$ ./rel/manager/bin/manager console
```


Documentation
-------------

```
cat rel/files/sys.config

...

{manager, [
	{ranch,  [{port, 8080}]},
	{cowboy, [{nb_acceptors, 100}, {protocol, [{compress, true}]}]},
	{routes, [
		{"/",             {priv_file, manager, "static/index.html"}},
		{"/assets/[...]", {priv_dir, manager, "static/assets"}},
		{"/images/[...]", {priv_dir, manager, "static/images"}},
		{"/css/[...]",    {priv_dir, manager, "static/css"}},
		{"/js/[...]",     {priv_dir, manager, "static/js"}},
		{"/v1/:action",   {request, my_http_module, my_http_handler}},
		{"/v2/:action",   {request, my_http_proto_module, my_http_proto_handler, sm_protocol_payload}},
		{"/v1/ws",        {websocket, my_websocket_module, sm_protocol_bert}}
	]}
]}
```


you can edit the **manager block** to change the cowboy listenner port and any other options.
and then execute in the console:

```
$ make clean && make relclean && make rel
$ ./rel/manager/bin/manager console 
```


Enjoy!



License
-------
Copyright 2015 - 2017 robertzhouxh. 
Under the [MIT](http://opensource.org/licenses/MIT) License.
