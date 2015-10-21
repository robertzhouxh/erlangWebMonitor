#!/bin/sh
# erl +K true +P 10240000 -sname testserver -pa ebin -pa deps/*/ebin -s myapp\
#   -eval "io:format(\"Server start with port 8000 Success!~n\")." \
#   > server.log

#erl +K true +P 10240000 -sname testserver -pa ebin -pa deps/*/ebin -s myapp
./rel/manager/bin/manager console
