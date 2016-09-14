#!/bin/sh

cat $1 | grep -Ev '[_:/\(\)=`<>{}]' | wc -w
