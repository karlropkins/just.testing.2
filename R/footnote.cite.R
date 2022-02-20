#########################
#' footnote.cite
#########################
#'
#' footnote.cite attempts to tidy citations so they can be used in document footnotes...
#' @param x The name of the package to be cited. The default cites R itself.
#' @param linebreak force linebreaks. See Note
#' @keywords methods
#' @note Linebreaking would typically be left to the code creating the footnote. However,
#' if you are using `footnote.cite()` in other applications `linebreak=TRUE` may help
#' @export
#' @examples
#' #to do

#kr v.0.1 11/08/2017
#still work to do
#check linebreak
#see muller/mller in fn.cite("dplyr")


footnote.cite <- function(x="", linebreak=FALSE){
  x <- gsub("\n", " ", gsub("_", "", format(if(x=="") citation() else
            citation(x), style="text")))
  if(linebreak){
    width <- getOption("width") * 0.95
    x <- strwrap(paste(x, collapse=""), width=width)
  }
  x
}
