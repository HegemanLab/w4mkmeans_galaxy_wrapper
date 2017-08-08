#!/bin/bash

# purge old output files
if [ -f test_log.txt ]; then rm test_log.txt; fi
if [ -f tool_test_output.html ]; then rm tool_test_output.html; fi
if [ -f tool_test_output.json ]; then rm tool_test_output.json; fi

# run test and write new output files, showing progress to command line output
planemo conda_install .
planemo test --conda_dependency_resolution w4mkmeans.xml 2>&1 | grep -v -i observer | tee test_log.txt

# summarize warnings and errors to command line output
grep -E "(ERROR)|(WARNING)" test_log.txt | grep -v samtools | grep -v "Dynamic Display Application links" | grep -v "twobit.loc"
