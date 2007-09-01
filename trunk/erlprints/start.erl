-module(start).
-export([boot/0, boot/1]).

boot() ->
    boot(true).
boot(false) ->
    compile();
boot(true) ->
    mysql_start(),
    compile().

mysql_start() ->
    erlydb:start(mysql, [{hostname, "localhost"},
			 {username, "printuser"},
			 {password, "printpass"},
			 {database, "erlprints"}]).

compile() ->
    erlyweb:compile(os:getenv("HOME")++
		    "/projects/code/erlang2facebook/erlprints",
		    [{erlydb_driver, mysql},
		     {auto_compile, true}]).
