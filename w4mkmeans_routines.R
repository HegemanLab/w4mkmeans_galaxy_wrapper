##------------------------------------------------------------------------------------------------------
## these are the batch-independent and file-structure-independent routines to support the w4mkmeans tool
##------------------------------------------------------------------------------------------------------

library(parallel)

w4kmeans_usage <- function() {
  return ( 
   paste(
     "w4mkmeans: bad input.",
     "# contract:",
     "    required - caller will provide an environment comprising:",
     "      log_print        - a logging function with the signature function(x, ...) expecting strings as x and ...",
     "      variableMetadata - the corresponding W4M data.frame having feature metadata",
     "      sampleMetdata    - the corresponding W4M data.frame having sample metadata",
     "      dataMatrix       - the corresponding W4M matrix",
     "      slots            - the number of parallel slots for calculating kmeans",
     "    optional - environment may comprise:",
     "      kfeatures        - an array of integers, the k's to apply for clustering by feature (default, empty array)",
     "      ksamples         - an array of integers, the k's to apply for clustering by sample (default, empty array)",
     "      iter.max         - the maximum number of iterations when calculating a cluster (default = 10)",
     "      nstart           - how many random sets of centers should be chosen (default = 1)",
     "      algorithm        - string from c('Hartigan-Wong', 'Lloyd', 'Forgy', 'MacQueen') (default = Hartigan-Wong)",
     "      ",
     "    this routine will return a list comprising:",
     "      variableMetadata - the input variableMetadata data.frame with updates, if any",
     "      sampleMetadata   - the input sampleMetadata data.frame with updates, if any",
     "      scores           - an array of strings, each representing a line of a tsv having the following header:",
     "                           clusterOn TAB k TAB totalSS TAB betweenSS TAB proportion",
     collapse = ifelse( .Platform$OS.type == "windows", "\r\n", "\n" ) 
    )
  )
}

w4mkmeans <- function(env) {
  # abort if 'env' is null or is not an environment
  if ( is.null(env) || ! is.environment(env) ) {
    stop(w4kmeans_usage())
  } 
  # supply default arguments
  if ( ! exists("iter.max" , env) ) env$iter.max  <- 10
  if ( ! exists("nstart"   , env) ) env$nstart    <- 1
  if ( ! exists("algorithm", env) ) env$algorithm <- 'Hartigan-Wong'
  if ( ! exists("ksamples" , env) ) env$ksamples  <- c()
  if ( ! exists("kfeatures", env) ) env$kfeatures <- c()
  # check mandatory arguments
  expected <- c(
    "log_print"
  , "variableMetadata"
  , "sampleMetadata"
  , "dataMatrix"
  , "slots"
  )
  # abort if any expected member of the environment is missing
  if ( ! Reduce(f = `&`, x = sapply(X = expected, FUN = exists, env), init = TRUE) ) {
    stop(w4kmeans_usage())
  } 
  # extract parameters from 'env'
  failure_action  <- env$log_print
  scores          <- c( "clusterOn\tk\ttotalSS\tbetweenSS\tproportion" )
  sampleMetadata  <- env$sampleMetadata
  featureMetadata <- env$variableMetadata
  ksamples        <- env$ksamples
  kfeatures       <- env$kfeatures
  slots           <- env$slots

  myLapply <- parLapply
  # uncomment the next line to mimic parLapply, but without parallelization (for testing/experimentation)
  # myLapply <- function(cl, ...) lapply(...)
  cl <- NULL
  if ( identical(myLapply, parLapply) ) {
    log_print(sprintf("w4mkmeans: using parallel evaluation with %d slots", slots))
    log_print(names(cl))
    cl <- makePSOCKcluster(names = slots)
    # from ?makePSOCKcluster: "It is good practice to shut down the workers by calling stopCluster."
    clusterExport(
      cl = cl
    , varlist = c(
        "tryCatchFunc"
      , "calc_kmeans_one_dimension_one_k"
      , "prepare.data.matrix"
      )
    )
    final <- function(cl) {
      # from ?makePSOCKcluster: "It is good practice to shut down the workers by calling stopCluster."
      if ( !is.null(cl) ) {
        log_print("w4mkmeans: stopping cluster used for parallel evaluation")
        stopCluster(cl)
      }
    }
  } else {
    log_print("w4mkmeans: using sequential evaluation (1 slot)")
    final <- function(cl) { }
  }

  tryCatch(
    expr = {
      # These myLapply calls produce lists of lists of results:
      #   - The outer list has no keys and its members are accessed by index
      #   - The inner list has keys "clusters" and "scores"

      # for each $i in ksamples, append column 'k$i' to data frame sampleMetadata
      ksamples_length <- length(ksamples)
      if ( ksamples_length > 0 ) {
        smpl_result_list <- myLapply( 
            cl = cl
          , ksamples
          , calc_kmeans_one_dimension_one_k
          , env = env
          , dimension = "samples"
          )
        for ( i in 1:ksamples_length ) {
          result <- smpl_result_list[[i]]
          if (result$success) {
            sampleMetadata[sprintf("k%d",ksamples[i])] <- result$value$clusters
            scores <- c(scores, result$value$scores)
          }
        }
      }

      # for each $i in kfeatures, append column 'k$i' to data frame featureMetadata
      kfeatures_length <- length(kfeatures)
      if ( kfeatures_length > 0 ) {
        feat_result_list <- myLapply( 
            cl = cl
          , kfeatures
          , calc_kmeans_one_dimension_one_k
          , env = env
          , dimension = "features"
          )
        for ( i in 1:kfeatures_length ) {
          result <- feat_result_list[[i]]
          if (result$success) {
            featureMetadata[sprintf("k%d",kfeatures[i])] <- result$value$clusters
            scores <- c(scores, result$value$scores)
          }
        }
      }

      return ( 
        list(
          variableMetadata = featureMetadata
        , sampleMetadata   = sampleMetadata  
        , scores           = scores          
        )
      )
    }
  , finally = final(cl)
  )
}

# calculate k-means for features or samples
#   - recall that the dataMatrix has features in rows and samples in columns
# return value:
#   list(clusters = km$cluster, scores = scores) 
# arguments:
#   env:
#     environment having dataMatrix
#   dimension:
#   - "samples":  produce clusters column to add to the sampleMetadata table
#     - this is the default case
#   - "variables":  produce clusters column to add to the variableMetadata table
#   k:
#     integer, the number of clusters to make
calc_kmeans_one_dimension_one_k <- function(k, env, dimension = "samples") {
  # abort if environment is not as expected
  if ( is.null(env) || ! is.environment(env) ) {
    stop("calc_kmeans_one_dimension_one_k - argument 'env' is not an environment")
  } 
  if ( ! exists("log_print", env) || ! is.function(env$log_print) ) {
    stop("calc_kmeans_one_dimension_one_k - argument 'env' - environment does not include log_print or it is not a function")
  } 
  # abort if k is not as expected
  if ( ! is.numeric(k) ) {
    stop("calc_kmeans_one_dimension_one_k - argument 'k' is not numeric")
  } 
  k <- as.integer(k)
  # abort if dimension is not as expected
  if (   ! is.character(dimension) 
      || ! Reduce( f =`|`, x = sapply(X = c("features","samples"), FUN = `==`, dimension), init = FALSE) ) {
    stop("calc_kmeans_one_dimension_one_k - argument 'dimension' is neither 'features' nor 'samples'")
  } 
  dm           <- env$dataMatrix
  iter.max     <- env$iter.max
  nstart       <- env$nstart
  algorithm    <- env$algorithm
  dim_features <- dimension == "features"
  # tryCatchFunc produces a list
  #   On success of expr(), tryCatchFunc produces
  #     list(success TRUE, value = expr(), msg = "")
  #   On failure of expr(), tryCatchFunc produces
  #     list(success = FALSE, value = NA, msg = "the error message")
  result_list <- tryCatchFunc( expr = function() {
    # kmeans clusters the rows; features are the columns of args_env$dataMatrix; samples, the rows
    # - to calculate sample-clusters, no transposition is needed because samples are rows
    # - to calculate feature-clusters, transposition is needed so that features will be the rows
    if ( ! dim_features ) dm <- t(dm)
    dm <- prepare.data.matrix( x.matrix = dm, data.transformation = function(x) { x } )
    # need to set.seed to get reproducible results from kmeans
    set.seed(4567)
    # do the k-means clustering
    km <- kmeans( x = dm, centers = k, iter.max, nstart = nstart, algorithm = algorithm )
    scores <-
      sprintf("%s\t%d\t%0.5e\t%0.5e\t%0.5f"
             , dimension
             , k
             , km$totss
             , km$betweenss
             , km$betweenss/km$totss
             )
    list(clusters = km$cluster, scores = scores)
  })
  return ( result_list )
}

# prepare.data.matrix - Prepare x.datamatrix for multivariate statistical analaysis (MVA)
#   - Motivation:
#     - Selection:
#       - You may want to exclude several samples from your analysis:
#         - If so, set the argument 'exclude.samples' to a vector of sample names
#       - You may want to exclude several features or features from your analysis:
#         - If so, set the argument 'exclude.features' to a vector of feature names
#     - Renaming samples:
#       - You may want to rename several samples from your analysis:
#         - If so, set the argument 'sample.rename.function' to a function accepting a vector 
#           of sample names and producing a vector of strings of equivalent length
#     - MVA is confounded by missing values.
#       - By default, this function imputes missing values as zero.
#       - For a different imputation, set the 'data.imputation' argument to a function
#         accepting a single matrix argument and returning  a matrix of the same
#         dimensions as the argument.
#     - Transformation
#       - It may be desirable to transform the intensity data to reduce the range.
#       - By default, this function performs an eigth-root transformation:
#         - Any root-tranformation has the advantage of never being negative.
#         - Calculation of the eight-root is four times faster in my hands than log10.
#         - However, it has the disadvantage that calculation of fold-differences 
#           is not additive as with log-transformation.
#           - Rather, you must divide the values and raise to the eighth power.
#       - For a different transformation, set the 'data.transformation' argument
#           to a function accepting a single matrix argument.
#         - The function should be written to return a matrix of the same dimensions
#           as the argument.
# arguments:
#   - x.matrix - matrix of intensities (or data.frame of sample metadata)
#     - one row per sample
#     - one column per feature or metadata attribute
#   - exclude.samples - vector of labels of matrix rows (samples) to omit from analysis
#   - exclude.features - vector of labels of matrix columnss (features) to omit from analysis
#   - sample.rename.function - function to be used to rename rows if necessary, or NULL
#     - e.g., sample.rename.function = function(x) {
#               sub("(.*)_.*","\\1", row.names(x))
#             }
#   - data.imputation - function applied to matrix to impute missing values
#     - e.g., data.imputation = function(m) {
#               m[is.na(m)] <- min(m, na.rm = TRUE) / 100
#               return (m)
#             }
#   - data.transformation - function applied to matrix cells
#     - e.g., data.transformation = function(x) { return( log10(x) ) }
#         or, data.transformation = log10
# result value:
#   transformed, imputed x.datamatrix with renamed rows and with neither excluded values nor features
#
################################
##
##  Notes regarding the effectiveness and performance of the data transformation method.
##
##  The two transformations that I tried (log10 and 8th root) required different imputation methods.
##
##  For the LCMS resin data set that I was working with, separation in MVA was nearly equivalent for:
##    data.imputation <- function(x.matrix) {
##      x.matrix[is.na(x.matrix)] <- 0
##      return (x.matrix)
##    }
##    data.transformation <- function(x) {
##      sqrt( sqrt( sqrt(x) ) )
##    }
##  and
##    data.imputation <- function(x.matrix) {
##      x.matrix[is.na(x.matrix)] <- min(x.matrix, na.rm = TRUE) / 100
##      return (x.matrix)
##    }
##    data.transformation <- function(x) {
##      log10(x)
##    }
##
##  Note further that triple application of the square root:
##  - may be four times faster than log10:
##  - may be three times faster than log2:
##
##      system.time( junk <- sqrt( sqrt( sqrt(1:100000000) ) ) )
##         user  system elapsed
##        0.832   0.236   1.069
##      system.time( junk <- log10(1:100000000) )
##         user  system elapsed
##        3.936   0.400   4.337
##      system.time( junk <- log2(1:100000000) )
##         user  system elapsed
##        2.784   0.320   3.101
##
################################
#
prepare.data.matrix <- function(
  x.matrix
, exclude.samples = NULL
, exclude.features = NULL
, sample.rename.function = NULL
, data.imputation =
    function(m) {
      # replace NA values with zero
      m[is.na(m)] <- 0
      # replace negative values with zero, if applicable (It should never be applicable!)
      if (min(m < 0)) {
        m <- matrix(lapply(X = m, FUN = function(z) {max(z,0)}), nrow = nrow(m) )
      }
      # return matrix as the result
      return (m)
    }
, data.transformation = function(x) {
    sqrt( sqrt( sqrt(x) ) )
  }
, en = new.env()
) {
  # MatVar - Compute variance of rows or columns of a matrix
  # ref: http://stackoverflow.com/a/25100036
  # For row variance, dim == 1, for col variance, dim == 2
  MatVar <- function(x, dim = 1) {
    if (dim == 1) {
      dim.x.2 <- dim(x)[2]
      if ( dim.x.2 == 0 )
        stop("MatVar: there are zero columns")
      if ( dim.x.2 == 1 ) {
        stop("MatVar: a single column is insufficient to calculate a variance")
        # return ( rep.int(x = 0, times = nrow(x)) )
      } else {
        return ( rowSums( (x    - rowMeans(x))^2 ) / ( dim(x)[2] - 1 ) )
      }
    } else if (dim == 2) {
      dim.x.1 <- dim(x)[1]
      if ( dim.x.1 == 0 ) {
        stop("MatVar: there are zero rows")
      }
      if ( dim.x.1 == 1 ) {
        stop("MatVar: a single row is insufficient to calculate a variance")
        # return ( rep.int(x = 0, times = ncol(x)) )
      } else {
        return ( rowSums( (t(x) - colMeans(x))^2 ) / ( dim(x)[1] - 1 ) )
      }
    } else stop("Please enter valid dimension, for rows, dim = 1; for colums, dim = 2")
  }

  nonzero.var <- function(x) {
    if (nrow(x) == 0) {
      print(str(x))
      stop("matrix has no rows")
    }
    if (ncol(x) == 0) {
      print(str(x))
      stop("matrix has no columns")
    }
    if ( is.numeric(x) ) {
      # exclude any rows with zero variance
      row.vars <- MatVar(x, dim = 1)
      nonzero.row.vars <- row.vars > 0
      nonzero.rows <- row.vars[nonzero.row.vars]
      if ( length(rownames(x)) != length(rownames(nonzero.rows)) ) {
        row.names <- attr(nonzero.rows,"names")
        x <- x[ row.names, , drop = FALSE ]
      }
      
      # exclude any columns with zero variance
      column.vars <- MatVar(x, dim = 2)
      nonzero.column.vars <- column.vars > 0
      nonzero.columns <- column.vars[nonzero.column.vars]
      if ( length(colnames(x)) != length(colnames(nonzero.columns)) ) {
        column.names <- attr(nonzero.columns,"names")
        x <- x[ , column.names, drop = FALSE ]
      }
    }
    return (x)
  }

  if (is.null(x.matrix)) {
    stop("FATAL ERROR - prepare.data.matrix was called with null x.matrix")
  }

  en$xpre <- x <- x.matrix

  # exclude any samples as indicated
  if ( !is.null(exclude.features) ) {
    my.colnames <- colnames(x)
    my.col.diff <- setdiff(my.colnames, exclude.features)
    x <- x[ , my.col.diff , drop = FALSE ]
  }

  # exclude any features as indicated
  if ( !is.null(exclude.samples) ) {
    my.rownames <- rownames(x)
    my.row.diff <- setdiff(my.rownames, exclude.samples)
    x <- x[ my.row.diff, , drop = FALSE ]
  }

  # rename rows if desired
  if ( !is.null(sample.rename.function) ) {
    renamed <- sample.rename.function(x)
    rownames(x) <- renamed
  }

  # save redacted x.datamatrix to environment
  en$redacted.data.matrix <- x

  # impute values missing from the x.datamatrix
  if ( !is.null(data.imputation) ) {
    x <- data.imputation(x)
  }

  # perform transformation if desired
  if ( !is.null(data.transformation) ) {
    x <- data.transformation(x)
  } else {
    x <- x
  }

  # purge rows and columns that have zero variance
  if ( is.numeric(x) ) {
    x <- nonzero.var(x)
  }

  # save imputed, transformed x.datamatrix to environment
  en$imputed.transformed.data.matrix <- x

  return(x)
}


##-----------------------------------------------
## helper functions for error detection/reporting
##-----------------------------------------------

# log-printing to stderr
log_print <- function(x, ...) { 
  cat(
    c(x, ...)
  , " at "
  , format(Sys.time(), "%a %d %b %Y %X")
  , "\n"
  , sep=""
  , file=stderr()
  )
}

# tryCatchFunc produces a list
#   On success of expr(), tryCatchFunc produces
#     list(success TRUE, value = expr(), msg = "")
#   On failure of expr(), tryCatchFunc produces
#     list(success = FALSE, value = NA, msg = "the error message")
tryCatchFunc <- function(expr) {
  # format error for logging
  format_error <- function(e) {
    paste(c("Error { message:", e$message, ", call:", e$call, "}"), collapse = " ")
  }
  my_expr <- expr
  retval <- NULL
  tryCatch(
    expr = {
      retval <- ( list( success = TRUE, value = my_expr(), msg = "" ) )
    }
  , error = function(e) {
      retval <<- list( success = FALSE, value = NA, msg = format_error(e) )
    }
  )
  return (retval)
}

# tryCatchProc produces a list
#   On success of expr(), tryCatchProc produces
#     list(success TRUE, msg = "")
#   On failure of expr(), tryCatchProc produces
#     list(success = FALSE, msg = "the error message")
tryCatchProc <- function(expr) {
  # format error for logging
  format_error <- function(e) {
    paste(c("Error { message:", e$message, ", call:", e$call, "}"), collapse = " ")
  }
  retval <- NULL
  tryCatch(
    expr = {
      expr()
      retval <- ( list( success = TRUE, msg = "" ) )
    }
  , error = function(e) {
      retval <<- list( success = FALSE, msg = format_error(e) )
    }
  )
  return (retval)
}

