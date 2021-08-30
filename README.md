# re_tuner

Library for tuning Regular Expressions string for [re](http://erlang.org/doc/man/re.html) module.

====================

An OTP library
--------------------
Create new library
-----

    $ rebar3  new lib re_tuner
	
Build
-----

    $ rebar3 compile

EUnit
-----

    $ rebar3 eunit -m shorthands_s_tests
	$ rebar3 eunit -m shorthands_w_tests
    $ rebar3 eunit -m shorthands_v_tests
	$ rebar3 eunit -m posix_alnum_tests
	$ rebar3 eunit -m posix_lower_tests
	$ rebar3 eunit -m posix_upper_tests
	$ rebar3 eunit -m posix_digit_tests
	$ rebar3 eunit -m posix_graph_tests
    $ rebar3 eunit -m posix_print_tests
	$ rebar3 eunit -m posix_punct_tests
    $ rebar3 eunit -m posix_space_tests
	$ rebar3 eunit -m posix_cntrl_tests
	$ rebar3 eunit -m posix_ascii_tests
	$ rebar3 eunit -m replace_tests
	$ rebar3 eunit -m mp_tests
	$ rebar3 eunit -m unicode_block_tests
	$ rebar3 eunit -m is_match_tests
	$ rebar3 eunit -m is_full_match_tests


EDoc
-----

    $ rebar3 edoc


Publishing on hex.pm
====================

hex.pm Publish project
-----
rebar3 hex publish

hex.pm Publish documentation
-----
    $ rebar3 hex docs
	
Make several commands one by one
-----	
    $ rebar3 do hex cut -i patch
	$ rebar3 do hex publish --yes, edoc, hex docs
	$ rebar3 do hex publish --yes --replace, edoc, hex docs