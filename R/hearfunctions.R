

hearlm<-function(lmfit, digits=2){
     tmp<-tempfile()
    sumfit<-summary(lmfit)
    writeLines(paste0("# model is ", paste(as.character(lmfit$call)[1:2], collapse = " "),
                     "\n\n",
                     "## coefficient estimates\n\n",
                     paste(paste(names(lmfit$coefficients),
                                 round(lmfit$coefficients,digits),sep = " "),
                           collapse = "\n\n"),"\n\n## p values\n\n",
                     paste(paste(names(lmfit$coefficients),
                                 round(sumfit$coefficients[,4],digits),sep = " "),
                           collapse = "\n\n"),
                     "\n\n## standard errors\n\n",
                     paste(paste(names(lmfit$coefficients),
                                 round(sumfit$coefficients[,2],digits),sep = " "),
                           collapse = "\n\n"),
                     "\n\n## r squared is ", round(sumfit$r.squared, digits ), sep=""),
               paste0(tmp, ".Rmd"))
    knit2html(paste0(tmp, ".Rmd"),output =paste0(tmp, ".html") )
    browseURL(paste0(tmp, ".html"))
}




