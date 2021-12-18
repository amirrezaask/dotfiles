#!/bin/bash
tool=$1
shift
query=`echo $@ | tr ' ' '+'`
url="cht.sh/$tool/$query?qT"
curl --no-progress-meter $url




