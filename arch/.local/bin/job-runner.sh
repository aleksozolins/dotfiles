#!/bin/bash

# Directory paths
JOB_DIR="/$HOME/Dropbox/jobs"
LOG_DIR="/$HOME/Dropbox/jobs/logs"

# Loop through all task files
for file in "$JOB_DIR"/*.job; do
  [ -e "$file" ] || continue # If no files exist, skip

  # Create a new log file in the log directory
  log_file="$LOG_DIR/$(basename "$file" .job).log"
  
  # Read the file line by line
  while IFS= read -r command || [ -n "$command" ]; do
    # Ignore empty lines
    if [ -z "$command" ]; then
      continue
    fi

    # Execute the command and capture output and error
    output=$(eval "$command" 2>&1)
    
    # Get current time
    timestamp=$(date +'%Y-%m-%dT%H:%M:%S')
    
    # Append the command, processed info, and output to the log file
    echo -e "\nCommand: $command" >> "$log_file"
    echo -e "Processed at: $timestamp\nOutput:\n$output\n" >> "$log_file"
    echo -e "===================================================================" >> "$log_file"

    # Wait for 2 seconds before processing the next command
    sleep 2
  done < "$file"
  
  # After processing, delete the original file
  rm "$file"
done
