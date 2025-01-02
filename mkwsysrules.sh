#!/bin/sh

sw_vers --productVersion | awk '{split($1,a,".");print(a[1])}'
