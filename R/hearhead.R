#' Gives an html table of the first few lines of the data frame along with a description of the variables
#' and dimension of the data frame.
#'
#' @param data.frame a data frame
#' @param nlines How many rows of the data frame will be shown in the table (default 6)
#' @param dec.places How many decimal places should be shown if the variable is numeric (default 3)
#'
#'
#' @return an html document with a table that has variable names, descriptions, and the first few lines of the data frame
#'
#' @examples
#' data(iris)
#' hearhead(iris)
#'
#' @export

hearhead<-function(data.frame, nlines=6,dec.places=3){
if(!is.data.frame(data.frame)){
  return(cat("argument data.frame not a data frame"))
}
dframename<-deparse(substitute(data.frame))
types<-sapply(data.frame,class)
for(i in 1:length(types)){
  if(types[i]=="factor"){
    levs<-levels(data.frame[,i])
    types[i]<-paste(types[i],"with", length(levs), "levels:", paste(levs, collapse = " "))
    data.frame[,i]<-as.character(data.frame[,i])
  }else{
    if(types[i]=="character"){
      levs<-names(table(data.frame[,i]))
      types[i]<-paste(types[i],"with", length(levs), "possible values:", paste(levs, collapse = " "))
    }else{
      if(types[i]=="numeric"){
        data.frame[,i]<-round(data.frame[,i],dec.places)
      }
    }

  }
}

tab<-rbind(names(types),rep("-", length(types)),types,data.frame[1:nlines,])

tmp<-tempfile()

writeLines( paste0(paste(c("# ", dframename, dim(data.frame)[1], "observations of",
                           dim(data.frame)[2], " variables",
                           "\n\n"),collapse=" "), paste(apply(tab, 1, paste, collapse="|"), collapse=" \n ")),
            paste0(tmp, ".Rmd"))
rmarkdown::render(input=paste0(tmp, ".Rmd"),output_file =paste0(tmp, ".html") ,output_format = "html_document")
browseURL(paste0(tmp, ".html"))
}
