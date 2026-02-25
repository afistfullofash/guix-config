#!/bin/sh
#
# ssh_repeat
# Repeat a ssh connection attempt untill it succedes then start a ssh conection
#
ssh_ping $1
ssh $1
