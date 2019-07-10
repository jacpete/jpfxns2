#' @title Adds a centered caption to a variety of output types in Rmarkdown.
#'
#' @description Adds a centered caption for latex pdf, html, and word (only left
#'   align works at this time) documents created using Rmarkdown.
#'   \code{adaptiveCenterCaption()} builds off of the function created using
#'   \code{captioner::captioner()}, assesses which type of output document is
#'   being called using \code{knitr::opts_knit$get("rmarkdown.pandoc.to")}, and
#'   then outputs plain markdown formating text for the chosen document type.
#'
#' @param chunckName character string. Rmarkdown code chunk name containing the
#'   graphic to label. Passed to the \code{name} option in the function created
#'   by \code{captioner::captioner()}.
#' @param caption character string. Caption body to add to graphic. Passed to
#'   the \code{caption} option in the function created by
#'   \code{captioner::captioner()}.
#' @param capFxn character string. Name of the function created by
#'   \code{captioner::captioner()} for that graphic type.
#' @param type character string. Type of output from the Rmarkdown knit. Likely
#'   not to be changed. Default is set to the result of
#'   \code{knitr::opts_knit$get("rmarkdown.pandoc.to")}
#'
#' @return Returns the plain text markdown code needed to create the centered
#'   caption for the specified output type.
#'
#' @section Creation notes: First created on 2019-Mar-26 as I was exploring how
#'   to use Rmarkdown.
#'
#' @examples
#' \donttest{
#' # Define captioner function
#' ```{r, include=FALSE}
#' figNum <- captioner::captioner(prefix="Fig.")
#' ```
#'
#' # Plot the figure with center alignment
#' ```{r Plot-Caret-Price, echo=FALSE, results='hide', fig.align='center'}
#' qplot(carat, price, data = diamonds)
#' ```
#'
#' # Run the adaptiveCenterCaption function making sure to include results="asis" in chunk options
#' ```{r, echo=FALSE, results="asis"}
#' adaptiveCenterCaption(chunckName="Plot-Caret-Price", caption = "This is caption 1", capFxn = "figNum")
#' ```
#'}
#'
#' @export
adaptiveCenterCaption <- function(chunckName, caption = "", capFxn, type = knitr::opts_knit$get("rmarkdown.pandoc.to")) {
  outputStyles <- c("latex", "html", "docx")

  switch(type,
         "latex" = cat(paste("\\begin{center}", do.call(capFxn, list(name = chunckName, caption = caption)), "\\end{center}", sep = "  \n")),
         "html" = cat(paste("<center>", do.call(capFxn, list(name = chunckName, caption = caption)), "</center>", sep = "")),
         "docx" = cat(do.call(capFxn, list(name = chunckName, caption = caption))),
         print("No type match found. Check output options for .Rmd and jpfxns2::adaptiveCenterCaption().")
  )
}




#' @title Latex Matrix
#'
#' @description Creates a matrix formated for latex with brackets in Rmarkdown.
#' Needs to be inserted "asis" inside of an existing formula by surrounding the
#' code chunk in \code{`$$`} signs or having this built into the output of the
#' code chunk (see Examples).
#'
#' @param mat numeric matrix.
#'
#' @return Returns the plain text latex code needed to create the formatted
#' matrix.
#'
#' @section Creation notes: First created on 2019-Jul-10 as I was working on
#' STAT 512 homework.
#'
#' @examples
#' \donttest{
#'  # Data
#'```{r}
#'Q1n <- 6
#'Q1Y <- matrix(c(1,2,4,3,3,2), ncol = 1)
#'Q1X0 <- rep(1,Q1n)
#'Q1X1 <- c(1,1,2,3,2,2)
#'Q1X2 <- c(2,3,6,3,2,4)
#'Q1X <- cbind(Q1X0, Q1X1, Q1X2)
#'```
#'
#' ### FIRST OPTION (Better when you need to make multiple matrices in equation)
#' # Start math formula outside chunck
#' $$\mathbf{X} =
#'
#' # First matrix
#'```{r echo=FALSE, results='asis'}
#'cat(adaptiveInlineMatrix(Q1X))
#'```
#'*
#' # Second matrix
#'```{r echo=FALSE, results='asis'}
#'cat(adaptiveInlineMatrix(Q1Y))
#'```
#'
#' # End math formula
#' $$
#'
#'
#' ### SECOND OPTION
#' # Define it all in one chuck (have to remember to do extra escape characters)
#'```{r echo=FALSE, results='asis'}
#'cat(paste0("$\mathbf{X} = ", adaptiveInlineMatrix(Q1X)), "$")
#'```
#'}
#' @export
adaptiveInlineMatrix <- function(mat) {
  latexArray <- character()
  for (r in 1:nrow(mat)) {
    for (c in 1:ncol(mat)) {
      if (c != ncol(mat)) {
        latexArray <- paste0(latexArray, mat[r,c], " & ")
      } else if (r != nrow(mat)) {
        latexArray <- paste0(latexArray, mat[r,c], " \\\\ ")
      } else {
        latexArray <- paste0(latexArray, mat[r,c], " ")
      }
    }
  }
  paste0("\\begin{bmatrix} ", latexArray, "\\end{bmatrix}")
}
