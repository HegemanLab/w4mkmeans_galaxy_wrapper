#!/bin/bash

planemo lint .

# run test and write new output files, showing progress to command line output
planemo conda_install .
planemo serve --conda_dependency_resolution .

