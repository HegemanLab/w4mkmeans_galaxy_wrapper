##-----------------------------------------------
## helper functions for error detection/reporting
##-----------------------------------------------

# log-printing to stderr
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

# format error for logging
format_error <- function(e) {
  paste(c("Error { message:", e$message, ", call:", e$call, "}"), collapse = " ")
}

# tryCatchFunc produces a list
#   func - a function that takes no arguments
#   On success of func(), tryCatchFunc produces
#     list(success TRUE, value = func(), msg = "")
#   On failure of func(), tryCatchFunc produces
#     list(success = FALSE, value = NA, msg = "the error message")
tryCatchFunc <- function(func) {
  retval <- NULL
  tryCatch(
    expr = {
      retval <- ( list( success = TRUE, value = func(), msg = "" ) )
    }
  , error = function(e) {
      retval <<- list( success = FALSE, value = NA, msg = format_error(e) )
    }
  )
  return (retval)
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
  # log to environment
  if ( !exists("log", envir = en) ) {
    en$log <- c()
  }
  enlog <- function(s) { en$log <- c(en$log, s); s }
  #enlog("foo")

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
      stop("matrix has no rows")
    }
    if (ncol(x) == 0) {
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

  enlog("prepare.data.matrix - get matrix")

  en$xpre <- x <- x.matrix

  # exclude any samples as indicated
  if ( !is.null(exclude.features) ) {
    enlog("prepare.data.matrix - exclude any samples as indicated")
    my.colnames <- colnames(x)
    my.col.diff <- setdiff(my.colnames, exclude.features)
    x <- x[ , my.col.diff , drop = FALSE ]
  }

  # exclude any features as indicated
  if ( !is.null(exclude.samples) ) {
    enlog("prepare.data.matrix - exclude any features as indicated")
    my.rownames <- rownames(x)
    my.row.diff <- setdiff(my.rownames, exclude.samples)
    x <- x[ my.row.diff, , drop = FALSE ]
  }

  # rename rows if desired
  if ( !is.null(sample.rename.function) ) {
    enlog("prepare.data.matrix - rename rows if desired")
    renamed <- sample.rename.function(x)
    rownames(x) <- renamed
  }

  enlog("prepare.data.matrix - save redacted x.datamatrix to environment")

  # save redacted x.datamatrix to environment
  en$redacted.data.matrix <- x

  # impute values missing from the x.datamatrix
  if ( !is.null(data.imputation) ) {
    enlog("prepare.data.matrix - impute values missing from the x.datamatrix")
    x <- data.imputation(x)
  }

  # perform transformation if desired
  if ( !is.null(data.transformation) ) {
    enlog("prepare.data.matrix - perform transformation")
    x <- data.transformation(x)
  } else {
    x <- x
  }

  # purge rows and columns that have zero variance
  if ( is.numeric(x) ) {
    enlog("prepare.data.matrix - purge rows and columns that have zero variance")
    x <- nonzero.var(x)
  }

  # save imputed, transformed x.datamatrix to environment
  en$imputed.transformed.data.matrix <- x

  return(x)
}

# vim: sw=2 ts=2 et :
