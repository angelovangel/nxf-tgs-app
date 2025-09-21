#!/usr/bin/env bash

# Usage: ./cleanup_miniserve.sh <dir> <days>
# Example: ./cleanup_miniserve.sh /home/serve-dir 2

MINISERVE_DIR="$1"
LOGFILE="logs/miniserve.log"
DAYS="${2:-14}" # Default to 14 days if not specified

find "$MINISERVE_DIR" -maxdepth 1 -type f -name "*.tar.gz" -mtime +"$DAYS" | while read -r file; do
  echo "$(date '+%Y-%m-%d %H:%M:%S') deleted $file (older than $DAYS)" >> "$LOGFILE"
  rm -f "$file"
done
