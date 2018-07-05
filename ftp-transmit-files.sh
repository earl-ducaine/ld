





#!/bin/bash
HOST='100.0.0.10'
USER='lispm'
# PASSWD='remotepasswd'
# bye
# ls -la
# list "lama:l.lispm;*.*"
# prompt
# list lama:l.lispm;*.*

ftp -d -n -v $HOST << EOT
ascii
user $USER

help dir

EOT
