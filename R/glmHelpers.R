#' @title Find overdispersion parameter
#'
#' @description \code{chat()} calculates the overdispersion parameter
#' \eqn{\hat{c}}.
#'
#' @param object Model object that your can be used with \code{residuals()} and
#' contains the vector \code{$df.residual}. See \code{Details}.
#' @param output logical of whether to print output to the console
#'
#' @details I know that this can work on \code{glm}, \code{MASS::glm.nb}, and
#' \code{pscl::zeroinfl} models.
#'
#' @return Returns a vector with of size 2 with the \eqn{\hat{c}} value and the
#' associated p-value from the \eqn{\chi^2} testing that the null that
#' \eqn{\hat{c}} = 1.
#'
#' @section Creation notes: First created on 2019-Nov-14 in Homework_4.Rmd while
#' working on homework for FNR647.
#'
#' @examples
#'
#' @export
chat <- function(object, output=TRUE) {
  pr <- residuals(object,"pearson")  #Pearson residuals from object out
  c.hat <- sum(pr^2)/object$df.residual
  c.hat[2] <- pchisq(sum(pr^2),object$df.residual, lower=FALSE)
  if (output) {
    cat("overdispersion ratio, chat, is ", c.hat[1], "\n")
    cat("p-value for null that chat = 1 is ", c.hat[2], "\n")
  }
  return(c.hat)
}


#' @title Model Coefficient Table
#'
#' @description \code{modelCoefTable()} creates a tibble of coefficients,
#' standard errors, Wald's z, and an associated p-value.
#'
#' @param model Model object. See \code{Details}.
#' @param oversdispersed logical of whether to recalculate standard error using
#' the overdispersion parameter \code{\link{chat}}.
#'
#' @details I know that this can work on \code{glm}, \code{MASS::glm.nb}, and
#' \code{pscl::zeroinfl} models.
#'
#' @return Returns a tibble of coefficients, standard errors, Wald's z, and an
#' associated p-value.
#'
#' @section Creation notes: First created on 2019-Nov-14 in Homework_4.Rmd while
#' working on homework for FNR647.
#'
#' @examples
#'
#' @export
modelCoefTable <- function(model, overdispersed = FALSE) {
  betas <- coefficients(model)
  stdErr <- sqrt(diag(vcov(model)))
  if (overdispersed) {
    c.hat <- chat(model, output = FALSE)[1]
    stdErr <- stdErr*sqrt(c.hat)
  }
  Wald <- betas/stdErr
  pvalue <- pchisq(Wald^2,1,lower.tail = FALSE)
  names(betas)
  tibble::tibble("Coefficient" = names(betas),
         "Estimate" = betas,
         "Std. Error" = stdErr,
         "z value" = Wald,
         "Pr(>|z|)" = pvalue)
}
