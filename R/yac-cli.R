#' yacas command line interface
#' 
#' Interactive interface to the yacas
#' 
#' The user types valid yacas input and presses return. Type 'quit' to return
#' to R prompt.
#' 
#' 
#' @param enable_history Use `R` history such that previous yacas commands can be used. Default is `TRUE`.
#' @return Output of yacas is returned.
#' @note Note that command will use R `history()` and modify it by default. 
#' Yacas is given a limited amount of time to complete, otherwise
#' `\[1\] CommandLine(1) : User interrupted calculation` is returned. E.g.
#' Taylor(x,0,5) 1/(1+x) will work, but Taylor(x,0,12) 1/(1+x) is likely to
#' take too long.
#' @references \url{http://yacas.sourceforge.net}
#' @keywords symbolmath
#' @examples
#' 
#' \dontrun{
#' yac_cli()
#'  (x+y)^3-(x-y)^3
#'  Simplify(%)
#'  q
#' }
#' 
#' @importFrom utils loadhistory savehistory
#' 
#' @concept yac_communication
#' 
#' @export
yac_cli <- function(enable_history = TRUE) {
    ## Enable history in yac_cli()
    ## https://stackoverflow.com/a/27528113
    if (enable_history == TRUE) {
        tmphistory <- tempfile()
        try(utils::savehistory(tmphistory), silent = TRUE)
        on.exit(unlink(tmphistory))
    }
    
    update_history <- function(x) {
        ## Update history:
        if (enable_history == TRUE) {
            histcon <- file(tmphistory, open = "a")
            writeLines(x, histcon)
            close(histcon)
            try(utils::loadhistory(tmphistory), silent = TRUE)
        }    
        invisible(x)
    }
  
    cat("Enter Yacas commands here. Type quit to return to R\n")
    x <- readline("Yacas->")
    
    while (length(which(c("stop;", "stop", "end;", "end", "quit;",
                          "quit", "exit;", "exit", "e;", "e", "q;", "q", "q()", "\n") == x)) ==
           0) {

               ## Put here to not include exit/quit/... in history
               update_history(x)
               
               o <- yac_str(x)
               print(o, quote=FALSE)
               
               x <- readline("Yacas->")
           }
}


    
    # Old:
    # x <- gsub("Out>","#",x)
    # x <- gsub(" ","",unlist(strsplit(x,"#"))[1])
    # x <- gsub(" ","",gsub("In>",'',x))
    # if (x != '' && !is.na(x)){
    #   pr <- try( parse(text = x ),silent=TRUE)
    #   if (class(pr)=='try-error'){
    #     # o<- yacas(x,retclass='character');
    #     o<- yacas(x)
    #   } else {
    #     o <- yacas(parse(text = x)); 
    #   }
    #   # o <- paste(o)
    #   # a<-lapply(o,function(s) cat(paste(s,"\n")))
    #   print(o)
    # }
