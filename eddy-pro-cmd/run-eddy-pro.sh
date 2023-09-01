#!/bin/bash

# this script runs eddy pro from the command line using the .eddypro and .metadata files in this directory

now=$(date +"%Y%m%d_%H%M%S")
log_file=logs/log-eddy-pro-cmd-$now.txt

echo "Starting eddy pro on the cmd line and sending logs to $log_file..."

eddypro_rp -s linux 3m.eddypro > $log_file
