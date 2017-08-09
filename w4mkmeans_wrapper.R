#!/usr/bin/env Rscript

# references:
#   what this does:
#   - [stats::kmeans](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)
#   - [stats::p.adjust](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html)
#   how this does what it does:
#   - [parallel::clusterApply](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/clusterApply.html)

# invocation:
#   Rscript w4mkmeans_wrapper.R \
#     algorithm "$algorithm" \
#     categorical_prefix "$categorical_prefix" \
#     data_matrix_path "$dataMatrix_in" \
#     iter_max "$iter_max" \
#     kfeatures "$kfeatures" \
#     ksamples "$ksamples" \
#     nstart "$nstart" \
#     sampleMetadata_out "$sampleMetadata_out" \
#     sample_metadata_path "$sampleMetadata_in" \
#     scores_out "$scores_out" \
#     slots "${GALAXY_SLOTS:-1}" \
#     variableMetadata_out "$variableMetadata_out" \
#     variable_metadata_path "$variableMetadata_in"
# 
# <inputs>
#   <param name="dataMatrix_in" label="Data matrix file" type="data" format="tabular" help="variable x sample, decimal: '.', missing: NA, mode: numerical, separator: tab" />
#   <param name="sampleMetadata_in" label="Sample metadata file" type="data" format="tabular" help="sample x metadata columns, separator: tab" />
#   <param name="variableMetadata_in" label="Variable metadata file" type="data" format="tabular" help="variable x metadata columns, separator: tab" />
#   <param name="categoricalPrefix" label="prefix for cluster names " type="text" value="k" help="Some tools require non-numeric values to discern categorical; e.g., enter 'k' here to prepend 'k' to cluster numbers in the output; default 'k'." />
#   <param name="kfeatures" label="K value(s) for features" type="text" value="0" help="Single or min,max value(s) for K for features (variables), or 0 for none." />
#   <param name="ksamples" label="K value(s) for samples" type="text" value="0" help="Single or min,max value(s) for K for samples, or 0 for none." />
#   <param name="iter_max" label="Max number of iterations" type="text" value="10" help="The maximum number of iterations allowed; default 10." />
#   <param name="nstart" label="Number of random sets" type="text" value="1" help="How many random sets should be chosen; default 1." />
# 	<param name="algorithm" label="Algorithm for clustering" type="select" value = "Hartigan-Wong" help="K-means clustering algorithm, default 'Hartigan-Wong'; alternatives 'Lloyd', 'MacQueen'; 'Forgy' is a synonym for 'Lloyd', see stats::kmeans reference for further info and references.">
# 	  <option value="Hartigan-Wong" selected="TRUE">Hartigan-Wong</option>
# 	  <option value="Lloyd">Lloyd</option>
# 	  <option value="MacQueen">MacQueen</option>
# 	  <option value="Forgy">Forgy</option>
# 	</param>
# </inputs>
# <outputs>
#   <data name="sampleMetadata_out" label="${tool.name}_${sampleMetadata_in.name}" format="tabular" ></data>
#   <data name="variableMetadata_out" label="${tool.name}_${variableMetadata_in.name}" format="tabular" ></data>
# </outputs>

##------------------------
## libraries for this file
##------------------------

library(batch) ## for 'parseCommandArgs'

##-------------------
## Pre-initialization
##-------------------

argVc <- unlist(parseCommandArgs(evaluate=FALSE))
if ( Reduce( `|`, grepl("tool_directory",names(argVc)) ) ) {
  tool_directory <- as.character(argVc["tool_directory"])
} else {
  tool_directory <- "."
}
r_path <- function(f) paste( tool_directory, f, sep = "/" )

##----------------------------------------------------------
## Computation - source general and module-specific routines
##----------------------------------------------------------

log_print <- function(x, ...) { 
  cat(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  , " "
  , c(x, ...)
  , "\n"
  , sep=""
  , file=stderr()
  )
}

# log_print(sprintf("tool_directory is %s", tool_directory))

w4m_general_purpose_routines_path <- r_path("w4m_general_purpose_routines.R")
# log_print(sprintf("w4m_general_purpose_routines_path is %s", w4m_general_purpose_routines_path))
if ( ! file.exists(w4m_general_purpose_routines_path) ) {
  log_print("cannot find file w4m_general_purpose_routines.R")
  q(save = "no", status = 1, runLast = TRUE)
}
# log_print("sourcing ",w4m_general_purpose_routines_path)
source(w4m_general_purpose_routines_path)
if ( ! exists("prepare.data.matrix") ) {
  log_print("'prepare.data.matrix' was not read from file w4m_general_purpose_routines.R")
  q(save = "no", status = 1, runLast = TRUE)
}

w4mkmeans_routines_path <- r_path("w4mkmeans_routines.R")
# log_print(sprintf("w4mkmeans_routines_path is %s", w4mkmeans_routines_path))
if ( ! file.exists(w4mkmeans_routines_path) ) {
  log_print("cannot find file w4mkmeans_routines.R")
  q(save = "no", status = 1, runLast = TRUE)
}
# log_print("sourcing ",w4mkmeans_routines_path)
source(w4mkmeans_routines_path)
if ( ! exists("w4mkmeans") ) {
  log_print("'w4mkmeans' was not read from file w4mkmeans_routines.R")
  q(save = "no", status = 1, runLast = TRUE)
}

##-----------------------------------------
## Computation - W4m data-suppport routines
##-----------------------------------------

# read_data_frame - read a w4m data frame from a tsv, with error handling
#   e.g., data_matrix_input_env <- read_data_frame(dataMatrix_in, "data matrix input")
read_data_frame <- function(file_path, kind_string, failure_action = log_print) {
  my.env <- new.env()
  my.env$success <- FALSE
  my.env$msg <- sprintf("no message reading %s", kind_string)
  tryCatch(
    expr = {
      my.env$data    <- utils::read.delim( fill = FALSE, file = file_path )
      my.env$success <- TRUE
    }
  , error = function(e) {
     my.env$msg <<- sprintf("%s read failed", kind_string)
    }
  )
  if (!my.env$success) {
    failure_action(my.env$msg)
  }
  return (my.env)
}

# write_result - write a w4m data frame to a tsv
write_result <- function(result, file_path, kind_string, failure_action = log_print) {
  my.env <- new.env()
  my.env$success <- FALSE
  my.env$msg <- sprintf("no message writing %s", kind_string)
  tryCatch(
    expr = {
      write.table(
        x = result
      , sep = "\t"
      , file = file_path
      , quote = FALSE
      , row.names = FALSE
      )
      my.env$success <- TRUE
    }
  , error = function(e) {
     my.env$msg <<- sprintf("%s write failed", kind_string)
    }
  )
  if (!my.env$success) {
    failure_action(my.env$msg)
    return (my.env)
  }
  return (my.env)
}

# read the three input files
read_input_data <- function(env, failure_action = log_print) {
  kind_string <- "none"
  tryCatch(
    expr = {
      # read in the sample metadata
      kind_string <- "sample metadata input"
      smpl_metadata_input_env <- 
        read_data_frame(
                         file_path = env$sample_metadata_path
                       , kind_string = kind_string
                       , failure_action = failure_action
                       )
      if (!smpl_metadata_input_env$success) {
        failure_action(smpl_metadata_input_env$msg)
        return ( FALSE )
      }
      env$sampleMetadata <- smpl_metadata_input_env$data

      # read in the variable metadata
      kind_string <- "variable metadata input"
      vrbl_metadata_input_env <- 
        read_data_frame(
                         file_path = env$variable_metadata_path
                       , kind_string = kind_string
                       , failure_action = failure_action
                       )
      if (!vrbl_metadata_input_env$success) {
        failure_action(vrbl_metadata_input_env$msg)
        return ( FALSE )
      }
      env$variableMetadata <- vrbl_metadata_input_env$data

      # read in the data matrix
      kind_string <- "data matrix input"
      data_matrix_input_env <-
        read_data_frame(
                         file_path = env$data_matrix_path
                       , kind_string = kind_string
                       , failure_action = failure_action
                       )
      if (!data_matrix_input_env$success) {
        failure_action(data_matrix_input_env$msg)
        return ( FALSE )
      }
      # data frame for dataMatrix has rownames in first column
      data_matrix_df <- data_matrix_input_env$data
      rownames(data_matrix_df) <- data_matrix_df[,1]
      data_matrix <- data_matrix_df[,2:ncol(data_matrix_df)]
      env$dataMatrix <- as.matrix(data_matrix)

    }
  , error = function(e) {
     failure_action( sprintf("read_input_data failed for '%s' - %s", kind_string, format_error(e)) )
     return ( FALSE )
    }
  )
  return ( TRUE )
}


read_input_failure_action <- function(x, ...) { 
  log_print("Failure reading input for '", modNamC, "' Galaxy module call")
  log_print(x, ...)
}

##--------------------------
## Computation - Entry Point
##--------------------------

##----------
## Constants
##----------

modNamC <- "w4mkmeans" ## module name

## options
##--------

# Set the handler for R error-handling
options( show.error.messages = F
       , error = function () { 
                   log_print( "Fatal error in '", modNamC, "': ", geterrmessage() )
                   q( "no", 1, F )
                 }
       , warn = -1
       )

# strings as factors? - not by default!
# save old value
strAsFacL <- options()$stringsAsFactors
options(stringsAsFactors = FALSE)


## log file
##---------

log_print("Start of the '", modNamC, "' Galaxy module call")

## arguments
##----------

args_env <- new.env()

# files

log_print("PARAMETERS (raw):")
invisible(
  lapply(
    X = 1:length(argVc)
  , FUN = function(i) {
      log_print(sprintf("  - %s: %s", names(argVc)[i], argVc[i]))
    }
  )
)

# write.table(as.matrix(argVc), col.names=F, quote=F, sep='\t')

## output files
sampleMetadata_out              <- as.character(argVc["sampleMetadata_out"])
variableMetadata_out            <- as.character(argVc["variableMetadata_out"])
scores_out                      <- as.character(argVc["scores_out"])
## input files
args_env$data_matrix_path       <- as.character(argVc["data_matrix_path"])
args_env$variable_metadata_path <- as.character(argVc["variable_metadata_path"])
args_env$sample_metadata_path   <- as.character(argVc["sample_metadata_path"])
  
# other parameters

# multi-string args - split csv: "1,2,3" -> c("1","2","3")
args_env$kfeatures <- strsplit(x = as.character(argVc['kfeatures']), split = ",", fixed = TRUE)[[1]]
args_env$ksamples  <- strsplit(x = as.character(argVc['ksamples' ]), split = ",", fixed = TRUE)[[1]]
# numeric args
args_env$iter_max  <- as.numeric(               argVc['iter_max'  ])
args_env$nstart    <- as.numeric(               argVc['nstart'   ])
args_env$slots     <- as.numeric(               argVc['slots'    ])
# string args
args_env$algorithm <- as.character(             argVc['algorithm'])
args_env$categorical_prefix <- as.character(    argVc['categorical_prefix'])


# make local 'log_print' function available through 'env'
args_env$log_print <- log_print

log_print("PARAMETERS (parsed):")
for (member in ls(args_env)) {
  value <- get(member, args_env)
  value <- ifelse(length(value) == 1, value, sprintf("c(%s)", paste(value, collapse=", ")))
  
  log_print(sprintf("  - %s: %s", member, ifelse( !is.function(value) , value, "function" )))
}
log_print("")

##---------------------------------------------------------
## Computation - attempt to read input data
##---------------------------------------------------------
if ( ! read_input_data(args_env, failure_action = read_input_failure_action) ) {
  result <- -1
} else {
  log_print("Input data was read successfully.")
  result <- w4mkmeans(env = args_env)
  log_print("returned from call to w4mkmeans.")
}

if ( length(result) == 0 ) {
  log_print("no results were produced")
  # exit with status code non-zero to indicate error
  q(save = "no", status = 1, runLast = FALSE)
} else if ( ! setequal(names(result),c("variableMetadata","sampleMetadata","scores")) ) {
  log_print(sprintf("unexpected result keys %s", names(result)))
  # exit with status code non-zero to indicate error
  q(save = "no", status = 1, runLast = FALSE)
} else if ( ! write_result(result = result$variableMetadata, file_path = variableMetadata_out, kind_string = "clustered variableMetadata")$success ) {
  log_print("failed to write output file for clustered variableMetadata")
  # exit with status code non-zero to indicate error
  q(save = "no", status = 1, runLast = FALSE)
} else if ( ! write_result(result = result$sampleMetadata, file_path = sampleMetadata_out, kind_string = "clustered sampleMetadata")$success ) {
  log_print("failed to write output file for clustered sampleMetadata")
  # exit with status code non-zero to indicate error
  q(save = "no", status = 1, runLast = FALSE)
} else {
  tryCatch(
    expr = {
      fileConn<-file(scores_out)
      writeLines(result$scores, fileConn)
      close(fileConn)
    }
  , error = function(e) {
      log_print(sprintf("failed to write output file for cluster scores - %s", format_error(e)))
      # exit with status code non-zero to indicate error
      q(save = "no", status = 1, runLast = FALSE)
    }
  )
}

##--------
## Closing
##--------


if (!file.exists(sampleMetadata_out)) {
  log_print(sprintf("ERROR %s::w4m_kmeans_wrapper - file '%s' was not created", modNamC, sampleMetadata_out))
}

if (!file.exists(variableMetadata_out)) {
  log_print(sprintf("ERROR %s::w4m_kmeans_wrapper - file '%s' was not created", modNamC, variableMetadata_out))
}

if (!file.exists(scores_out)) {
  log_print(sprintf("ERROR %s::w4m_kmeans_wrapper - file '%s' was not created", modNamC, scores_out))
}

log_print("Normal termination of '", modNamC, "' Galaxy module call")

# exit with status code zero
q(save = "no", status = 0, runLast = FALSE)
