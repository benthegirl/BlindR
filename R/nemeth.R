#' Converts math to nemeth for braille
#'
#' Currently only works for functions in x and y
#'
#' @param mathstring a character string which contains math
#'
#'
#' @return character string with math in nemeth
#'
#' @examples
#'
#' @export

# mathstring<-"y=(.55*x^4+0.916*x^(1/2)+1/2*x+1/(2*x)+x^-(2/2)+x+1/2)/2"
# mathstring<-gsub(pattern = "\\*", replacement = "",mathstring)
#
# regmatches(mathstring, gregexpr("/", mathstring)[1], invert=T)
#
#
# #mathstring<-gsub(pattern = "\\^", replacement = "~", mathstring)
# mathstring<-gsub(pattern = "=", replacement = "\u2828\u2805", mathstring)
#
# while(sum(grep(pattern="/",x = mathstring))>0){
#
#
# pattern<-"-?(\\d+|\\d+\\.\\d+|\\.\\d+)"
#
# gregexpr(pattern = pattern, mathstring)
#
# is.number<-function(
#
# mathstring
#
# x <- c("A and B", "A, B and C", "A, B, C and D", "foobar")
# pattern <- "[[]]*(,|and)[[:space:]]"
# ## Match data from regexpr()
# m <- regexpr(pattern, x)
# regmatches(x, m)
# regmatches(x, m, invert = TRUE)
# ## Match data from gregexpr()
# m <- gregexpr(pattern, x)
# regmatches(x, m)
# regmatches(x, m, invert = TRUE)


nemeth <- function(expr_str) {

    expr_type <- function(x) {
        if (rlang::is_syntactic_literal(x)) {
            if(is.numeric(x)){
              "number"
            }else{
          "constant"
            }
        } else if (is.symbol(x)) {
            "symbol"
        } else if (is.call(x)) {
            "call"
        } else if (is.pairlist(x)) {
            "pairlist"
        } else {
            typeof(x)
        }
    }

    render <- function(e) {
        switch(
            expr_type(e),
            'number'= sub(pattern = "^0.",replacement = ".",formatC(e, digits=8, format="f", drop0trailing = T)),
            'constant' = paste(e),
            'symbol' = render_symbol(e),
            'call' = render_call(e)
        )
    }

    render_symbol <- function(e) {
        sym <- as.character(e)
        symmap = list('beta' = '.b')
        if (sym %in% names(symmap)) {
            return(symmap[[sym]])
        }
        return(sym)
    }

    render_call <- function(e) {
        fnmap <- list('=' = ' .k ',
                      '/' = './')
        fn <- as.character(e[[1]])
        if (fn == '*') {
            ## Multiplication stuff
            left <- render(e[[2]])
            right <- render(e[[3]])
            if (grepl('^[.0-9]', right)) {
                ## two numerical things, explicit multiplication symbol
                return(paste(left, '*', right, sep = ''))
            } else {

                ## at least one non-numeric, so let's go implicit.
                return(paste(left, right, sep = ''))
            }
        } else if (fn == '(') {
            ## Parens
            return(paste('(', render(e[[2]]), ')', sep = ''))
        } else if (fn == '^'){

            return(paste(render(e[[2]]), fn, render(e[[3]]),"\U201D", sep = ''))
        } else if (fn %in% c('+', '-', '/', '=')) {
            ## Other operators
            if (fn %in% names(fnmap)) {
                ## Map operator character as needed.
                fn <- fnmap[[fn]]
            }
            if (length(e) == 3) {
                # infix
                return(paste(render(e[[2]]), fn, render(e[[3]]), sep = ''))
            } else {
                # prefix
                return(paste(fn, render(e[[2]]), sep = ''))
            }
        }
    }

    mystring<-render(rlang::parse_expr(expr_str))
    if(substr(mystring, nchar(mystring),nchar(mystring))==";")
      return(substr(mystring, start=1, stop=(nchar(mystring)-1)))
    else(
      return(mystring)
    )
}
