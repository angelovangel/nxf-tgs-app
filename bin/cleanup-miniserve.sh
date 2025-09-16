#!/usr/bin/env bash

# Usage: ./cleanup_miniserve.sh <days>
# Example: ./cleanup_miniserve.sh 2

MINISERVE_DIR="miniserve"
LOGFILE="$MINISERVE_DIR/miniserve.log"
NUM="${1:-2}" # Default to 2 days if not specified
UNITS="${2:-d}" # Default to days if not specified

find "$MINISERVE_DIR" -maxdepth 1 -type f -name "*.tar.gz" -mtime +"$NUM""$UNITS" | while read -r file; do
  echo "$(date '+%Y-%m-%d %H:%M:%S') deleted $file (older than $NUM $UNITS)" >> "$LOGFILE"
  rm -f "$file"
done
