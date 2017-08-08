##------------------------------------------------------------------------------------------------------
## these are the batch-independent and file-structure-independent routines to support the w4mkmeans tool
##------------------------------------------------------------------------------------------------------

library(parallel)

w4kmeans_usage <- function() {
  return ( 
    c(
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
     "                           clusterOn TAB k TAB totalSS TAB betweenSS TAB proportion"
    )
  )
}

w4mkmeans <- function(env) {
  # abort if 'env' is null or is not an environment
  if ( is.null(env) || ! is.environment(env) ) {
    lapply(w4kmeans_usage(),print)
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
  missing_from_env <- setdiff(expected, (ls(env)))
  if ( length(missing_from_env) > 0 ) {
    print(paste(c('expected environment members not found: ', as.character(missing_from_env)), collapse = ", "))
    lapply(w4kmeans_usage(),print)
    stop("w4mkmeans: contract has been broken")
  } 
  # extract parameters from 'env'
  failure_action  <- env$log_print
  scores          <- c( "clusterOn\tk\ttotalSS\tbetweenSS\tproportion" )
  sampleMetadata  <- env$sampleMetadata
  featureMetadata <- env$variableMetadata
  ksamples        <- as.numeric(env$ksamples)
  kfeatures       <- as.numeric(env$kfeatures)
  slots           <- env$slots

  myLapply <- parLapply
  # uncomment the next line to mimic parLapply, but without parallelization (for testing/experimentation)
  # myLapply <- function(cl, ...) lapply(...)
  cl <- NULL
  if ( identical(myLapply, parLapply) ) {
    failure_action(sprintf("w4mkmeans: using parallel evaluation with %d slots", slots))
    failure_action(names(cl))
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
        failure_action("w4mkmeans: stopping cluster used for parallel evaluation")
        stopCluster(cl)
      }
    }
  } else {
    failure_action("w4mkmeans: using sequential evaluation (1 slot)")
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
    stop(sprintf("calc_kmeans_one_dimension_one_k - expected numeric argument 'k' but type is %s", typeof(k)))
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

