#' Converts text with numbers to have number symbol in correct place
#'
#'
#' @param lab A text string
#'
#'
#' @return the text labels with number symbols added
#'
#' @examples
#' numberfixer("Here is a number: 77")
#' numberfixer("meet 1e+05")
#' numberfixer("negatives are okay -75")
#' numberfixer("so are decimals -.5")
#' numberfixer("also -0.2")
#'
#' @export

numberfixer <- function(lab) {
  string<-lab
    ## # in front of numbers
    string <- gsub('((?:^|\\s)-?)([.0-9]+)', '\\1#\\2', string)
    ## Clean up scientific notation this gives rspeak
   # string <- gsub('(#[.0-9]+e)[+]?([-]?)0*([0-9]+)', '\\1\\2\\3', string)
    ## Clean up scientific notation
    string <- gsub('(#[.0-9]+)e[+]?([-]?)0*([0-9]+)', '\\1@*10^\\2\\3', string)
    ## Fix up colons
    string <- gsub(':', '3', string)
    return(string)
}
