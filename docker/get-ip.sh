#!/bin/bash
ifconfig eth0 | perl -ne '/inet addr:(\S+)/ && print "$1"'
