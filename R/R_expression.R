as.R.expression.yacas<-function(y) return(parse(text=y[["text"]]))
yacas.R.expression<-function(x) as.R.expression.yacas(yacas(x))
