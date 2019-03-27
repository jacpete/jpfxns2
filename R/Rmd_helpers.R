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
