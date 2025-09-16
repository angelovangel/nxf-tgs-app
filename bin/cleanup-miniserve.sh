#!/usr/bin/env bash

# Usage: ./cleanup_miniserve.sh <days>
# Example: ./cleanup_miniserve.sh 2

MINISERVE_DIR="miniserve"
LOGFILE="logs/miniserve.log"
DAYS="${1:-14}" # Default to 14 days if not specified

find "$MINISERVE_DIR" -maxdepth 1 -type f -name "*.tar.gz" -mtime +"$DAYS" | while read -r file; do
  echo "$(date '+%Y-%m-%d %H:%M:%S') deleted $file (older than $DAYS)" >> "$LOGFILE"
  rm -f "$file"
done
