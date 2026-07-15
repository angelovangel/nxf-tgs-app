#!/usr/bin/env bash

# Usage: ./cleanup_miniserve.sh <dir> <days>
# Example: ./cleanup_miniserve.sh /home/serve-dir 2

source secrets

MINISERVE_DIR="$1"
LOGFILE="logs/miniserve.log"
DAYS="${2:-14}" # Default to 14 days if not specified

find "$MINISERVE_DIR" -maxdepth 1 -type f -name "*.tar.gz" -mtime +"$DAYS" | while read -r file; do
  echo $file;
  if curl --fail -T "$file" -u "$USERNAME:$PASS" "$URL/$STORAGE/"; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') $file transferred OK" >> "$LOGFILE"
    rm -f "$file" && echo "$(date '+%Y-%m-%d %H:%M:%S') $file deleted (older than $DAYS days)" >> "$LOGFILE"
  else
    echo "$(date '+%Y-%m-%d %H:%M:%S') $file transfer failed! File NOT deleted." >> "$LOGFILE"
  fi
done
