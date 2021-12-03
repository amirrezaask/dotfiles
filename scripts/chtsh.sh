#!/bin/bash
tool=$1
shift
query=`echo $@ | tr ' ' '+'`
url="cht.sh/$tool/$query"
echo $url
curl $url




