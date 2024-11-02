#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <script_path> <start_number> <end_number>"
  exit 1
fi

# Assign arguments to variables
SCRIPT_PATH=$1
START_NUM=$2
END_NUM=$3

# Loop through the given range and run the R script
for i in $(seq $START_NUM $END_NUM)
do
  Rscript $SCRIPT_PATH $i
done
