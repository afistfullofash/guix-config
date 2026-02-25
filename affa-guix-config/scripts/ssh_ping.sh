#!/bin/sh
#
# ssh_ping
# Loop a connection attempt to the ssh host at <host>
# When it is finished return a message with the current time
#
starting_time=$(date)
while ! ssh $1 true; do
  sleep 5
done;
echo "Host is back up"
echo "Starting: ${starting_time}"
echo "Ending: $(date)"
