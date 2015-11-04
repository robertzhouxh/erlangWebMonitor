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

### Ubuntu

```
$ apt-get update

# Install the build tools (dpkg-dev g++ gcc libc6-dev make)
apt-get -y install build-essential

# automatic configure script builder (debianutils m4 perl)
apt-get -y install autoconf

# Needed for HiPE (native code) support, but already installed by autoconf
# apt-get -y install m4

# Needed for terminal handling (libc-dev libncurses5 libtinfo-dev libtinfo5 ncurses-bin)
apt-get -y install libncurses5-dev

# For building with wxWidgets
apt-get -y install libwxgtk2.8-dev libgl1-mesa-dev libglu1-mesa-dev libpng3

# For building ssl (libssh-4 libssl-dev zlib1g-dev)
apt-get -y install libssh-dev

# ODBC support (libltdl3-dev odbcinst1debian2 unixodbc)
apt-get -y install unixodbc-dev

$ wget http://www.erlang.org/download/otp_src_18.1.tar.gz
$ tar zxvf otp_src_18.1.tar.gz
$ ./configure && make && sudo make install
$ erl

...

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
**configure the mysql and redis**

1) in ./deps/emysql/src/emysql.app.src :

modify the host and the information of database and table in MySQL, like this:

`````````````````````````````````````````
{application, emysql,
 [
  {description, "Erlang MySQL Client"},
  {vsn, "4.0"},
  {modules, []},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto]},
  {env, [
        {pool, 4},
        {host, "IP"},
        {port, 3306},
        {username, "xxxxxxx"},   %% username in MySQL
        {password, "xxxxxxx"},   %% Password
        {database, "xxxxxxx"},   %% name of database
        {encoding, utf8}
        ]
   },
  {mod, {emysql_app, []}}
]}.

`````````````````````````````````````````

2) in ./deps/eredis_pool/src/eredis_pool.app.src :

add your own pool and relative message, especially in file eredis_pool.app.arc, you maybe change tuple {global_or_local, local/global}, i.e. "local" means to work for localhost, "global" means to work for all host. like this:

`````````````````````````````````````````
{application, eredis_pool,
 [
  {description, ""},
  {vsn, "1.1"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { eredis_pool_app, []}},
  {env, [
         {global_or_local, global},
         {pools, [
                  {default, [
                             {size, 10},
                             {max_overflow, 20}
                            ], [
                              {host, "IP"},   %% input ip
                              {port, 6379}
                             ]},
                  {pool1, [
                           {size, 30},
                           {max_overflow, 20}
                          ], [
                              {host, "IP"},   %% input ip
                              {port, 6379}
                             ]}
                  
                 ]}
        ]}
]}.


`````````````````````````````````````````

you can edit the **manager block** to change the cowboy listenner port and any other options.
and then execute in the console:

```
$ make clean && make relclean && make rel
$ ./rel/manager/bin/manager console
```
API
------------
/v2/login   POST     login, and you should set your usename and Password when you login first time. when you enter the console, you should run "manager_app:set_auth(yourusername:yourpassword)". 
/v2/logout  GET      Logout
/v2/users   GET      get users Information
/v2/online  GET      get information of online users
/v2/devices GET      get information of devices (including total devices, total public devices, new devices and new public devices in some duration). 

Enjoy!



License
-------
Copyright 2015 - 2017 robertzhouxh.
Under the [MIT](http://opensource.org/licenses/MIT) License.
