#!/bin/bash

        dataMatrix_in="test-data/input_dataMatrix.tsv"
  variableMetadata_in="test-data/input_variableMetadata.tsv"
    sampleMetadata_in="test-data/input_sampleMetadata.tsv"
             ksamples="3,4"
            kfeatures="5,6,7"
              iter_max="10"
               nstart="1"
            algorithm="Hartigan-Wong"
           scores_out="test-data/output_scores.tsv"
   sampleMetadata_out="test-data/output_sampleMetadata.tsv"
 variableMetadata_out="test-data/output_variableMetadata.tsv"
         GALAXY_SLOTS=3

Rscript w4mkmeans_wrapper.R \
data_matrix_path "$dataMatrix_in" \
variable_metadata_path "$variableMetadata_in" \
sample_metadata_path "$sampleMetadata_in" \
ksamples "$ksamples" \
kfeatures "$kfeatures" \
iter_max "$iter_max" \
nstart "$nstart" \
algorithm "$algorithm" \
scores_out "$scores_out" \
sampleMetadata_out "$sampleMetadata_out" \
variableMetadata_out "$variableMetadata_out" \
slots "${GALAXY_SLOTS:-1}"


