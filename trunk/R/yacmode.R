
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


