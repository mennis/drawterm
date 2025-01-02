#!/bin/sh

sw_vers | awk '/ProductVersion/{split($2,a,".");print(a[2])}'
