#!/bin/bash

        dataMatrix_in="test-data/input_dataMatrix.tsv"
  variableMetadata_in="test-data/input_variableMetadata.tsv"
    sampleMetadata_in="test-data/input_sampleMetadata.tsv"
             ksamples="3,4"
            kfeatures="5,a,7,6.5,6"
              iter_max="10"
               nstart="1"
            algorithm="Hartigan-Wong"
    categoricalPrefix="_"
           scores_out="test-data/output_scores.tsv"
   sampleMetadata_out="test-data/output_sampleMetadata.tsv"
 variableMetadata_out="test-data/output_variableMetadata.tsv"
         GALAXY_SLOTS=3

Rscript w4mkmeans_wrapper.R \
  algorithm "$algorithm" \
  categorical_prefix "$categoricalPrefix" \
  data_matrix_path "$dataMatrix_in" \
  iter_max "$iter_max" \
  kfeatures "$kfeatures" \
  ksamples "$ksamples" \
  nstart "$nstart" \
  sampleMetadata_out "$sampleMetadata_out" \
  sample_metadata_path "$sampleMetadata_in" \
  scores_out "$scores_out" \
  slots "${GALAXY_SLOTS:-1}" \
  variableMetadata_out "$variableMetadata_out" \
  variable_metadata_path "$variableMetadata_in"
