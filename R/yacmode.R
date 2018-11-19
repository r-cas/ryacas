#' yacmode interface
#' 
#' Interactive interface to the yacas
#' 
#' The user types valid yacas input and presses return. Type 'quit' to return
#' to R prompt.
#' 
#' @return Output of yacas is returned.
#' @note Note that command recall will recall previous R commands, not previous
#' yacas input. Yacas is given a limited amount of time to complete, otherwise
#' `\[1\] CommandLine(1) : User interrupted calculation` is returned. E.g.
#' Taylor(x,0,5) 1/(1+x) will work, but Taylor(x,0,12) 1/(1+x) is likely to
#' take too long.
#' @references \url{http://yacas.sourceforge.net}
#' @keywords symbolmath
#' @examples
#' 
#' \dontrun{
#' yacmode()
#'  (x+y)^3-(x-y)^3
#'  Simplify(%)
#'  q
#' }
#' 
#' @export
yacmode <-function (){
    cat("Enter Yacas commands here. Type quit to return to R\n")
    x <- readline("Yacas->")
    while (length(which(c("stop;", "stop", "end;", "end", "quit;",
                          "quit", "exit;", "exit", "e;", "e", "q;", "q","\n") == x)) ==
           0) {
      
      x <- gsub("Out>","#",x)
      x <- gsub(" ","",unlist(strsplit(x,"#"))[1])
      x <- gsub(" ","",gsub("In>",'',x))
      if (x != '' && !is.na(x)){
        pr <- try( parse(text = x ),silent=TRUE)
        if (class(pr)=='try-error'){
          # o<- yacas(x,retclass='character');
          o<- yacas(x)
        } else {
          o <- yacas(parse(text = x)); 
        }
        # o <- paste(o)
        # a<-lapply(o,function(s) cat(paste(s,"\n")))
        print(o)
      }    
      x <- readline("Yacas->")
    }
  }


