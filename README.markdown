What is FreeSWITCH EventCool?
=================================
FreeSWITCH EventCool is a simple tool to collect FreeSWITCH events via mod\_erlang\_event and display on a web page.

To use this, you need build FreeSWITCH with mod\_erlang\_event

	cd $(FreeSWITCH source dir)
	make mod_erlang_event install

And load the mod in FreeSWITCH:

	fs_cli> load mod_erlang_event

Config freeswitch\_node in boss.config.

It using the Chicago Boss framework, you just need to git clone the code and `make`(see below for more detail about Chicago Boss).

You also need to change the start script, see start\_seven.sh for a sample.

**Note**: It using the default fake db driver, so it's not persistent. See Chicago Boss documents if you want persistent store.

See this link for more info:
<http://www.dujinfang.com/past/2010/12/9/freeswitch-eventcool/>

TODO
--------

* order by uuid
* more fan...

Getting Started With Chicago Boss
=================================

Quickstart
----------

Dependencies:

* Erlang R13A or later -

    <http://www.erlang.org/download.html>

  * Check with `erlang:system_info(otp_release)`.

Build Chicago Boss with `make`.

* On Windows Vista or Windows 7 -

    1. you need install win openSSl (http://www.slproweb.com/products/Win32OpenSSL.html)
    2. make mochiweb with msys or cygwin

When you're ready to rock and roll, run `./START-DEV.SH` (or start-dev.bat) in this directory (admin priveleges needed).
There will be a lot of PROGRESS REPORTs which look scary but hopefully
everything is running smoothly. With this console you can interact directly
with the running server. Next, point your browser to:

    http://localhost:8001/

If all is well you will see "Hello, World!" Now you can get busy. 


Documentation
-------------

See the FAQ and API files located at

<http://www.chicagoboss.org/>

If you need help getting started, check out "An Evening With Chicago Boss":

<http://www.evanmiller.org/chicago-boss-guide.html>

There's also the mailing list:

<http://groups.google.com/group/chicagoboss>
