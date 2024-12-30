#!/bin/bash

title="ptest"

# make oat_experiments FILE=hw4programs/regalloctest.oat
# make oat_experiments FILE=hw4programs/regalloctest.oat OPT=-O1

# make ll_experiments FILE=llprograms/matmul.ll
# make ll_experiments FILE=llprograms/matmul.ll OPT=-O1

make oat_experiments FILE=test/$title.oat
make oat_experiments FILE=test/$title.oat OPT=-O1

programs=(
    "a_baseline.out" "a_greedy.out" "a_better.out" "a_clang.out"
    "a_baseline-O1.out" "a_greedy-O1.out" "a_better-O1.out" "a_clang-O1.out"
)

output_file="$title-time_results.txt"

echo "Running Test for $title" > "$output_file"

for i in {1..5}; do
  echo "Running iteration $i..." >> "$output_file"
  for program in "${programs[@]}"; do
    # { time ./$program; } 2>> "$output_file"
    { time ./$program; } 2>&1 | grep real | awk '{print $2}' >> "$output_file"
  done
  echo "" >> "$output_file"
done



echo "All done! Results saved in $output_file."