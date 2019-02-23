# by Jacob Peterson
# created 2-10-19
# first created in FIA_Test.R for BigDataFNR Project in purdueResearch project

# Will be useful subsetting large dataframes to run certain bits through
# an lapply and then using dplr::bind_rows to group them back together
# Originally helped speed up pivot tables in the original script

# A handy function for splitting dataframes ====================================
splitDF <- function(data, sep=1000,byCol=NULL) {
  if (is.null(byCol)) { # if you want to seperate by rows in dataframe
    start <- seq(1, nrow(data), sep)
    end <- seq(sep, nrow(data), sep)
    if (end[length(end)] < nrow(data)) {
      end <- c(end, nrow(data))
    }
  } else { # else seperate by supplied column name
    colVals <- unique(data[,byCol])
    start <- seq(1, length(colVals), sep)
    end <- seq(sep, length(colVals), sep)
    if (end[length(end)] < length(colVals)) {
      end <- c(end, length(colVals))
    }
  }

  ##Split the data by either rows or groups given from byCol variable
  output <- lapply(seq_along(start), function(x) {
    if (is.null(byCol)) {
      out <- data[start[x]:end[x],]
    } else {
      out <- data[data[,byCol] %in% colVals[start[x]:end[x]],]
    }
    return(out)
  })
}
