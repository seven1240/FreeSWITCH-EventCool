#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -boot start_sasl -config boss -s boss -s reloader -setcookie ClueCon -name fs_events@seven-macpro.local
