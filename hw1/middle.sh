#!/bin/sh
# Select lines from the middle of a file.
# Usage: bash middle.sh filename endline num_lines
head -n "$2" "$1" | tail -n "$3"
