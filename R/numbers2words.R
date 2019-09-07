#I got this from https://gist.github.com/psychemedia/150cb9901529da58124a and
#  added to my helper package because I want it it be easier for me to access
#  without sourcing a script.
#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
#Function by John Fox found here:
# http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
# Tweaks by AJH to add commas and "and"

#' @title Integer numbers to words
#' @description \code{numbers2words()} takes an integer argument and outputs
#' it in word form.
#'
#' @param x Input integer.
#'
#' @details See description.
#'
#' @return Returns a character string.
#'
#' @section Creation notes: First added on 2019-Sept-7 while trying to finish
#' the first homework assignment for FNR647. I did not create the underlying
#' code. This credit goes as follows:
#'
#' I got this from https://gist.github.com/psychemedia/150cb9901529da58124a and
#'  added to my helper package because I want it it be easier for me to access
#'  without sourcing a script.
#'
#' The author AJH edited from:
#'  https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
#'
#' Who accredits John Fox as the original author
#'  Function by John Fox found here:
#'  http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
#'
#' @export


numbers2words <- function(x){
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){

    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  #Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}
