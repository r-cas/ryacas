trans <- function(x, ttab=transtab, from, to) {
   idx <- match(x, ttab[,from], nomatch = 0)
   res <- if (idx > 0) ttab[idx,to] else x
   if (tolower(substr(res, 1, 1)) %in% letters) res
   else paste('"', res, '"', sep="")
}

transtab <- matrix( c(
	#R			yacas
	"pi",		"Pi",
	
	"+",		"+",
	"-",		"-",
	"*",		"*",
	"/",		"/",
	"/",		"/",
	"^",		"^",
	"%%",		"Mod",
	"%/%",		"Div",
	"root",		"NthRoot",
	"Inf",		"Infinite",
	"NaN",		"Undefined",
	
	"sin",		"Sin",
	"cos",		"Cos",
	"tan",		"Tan",
	
	"asin",		"ArcSin",
	"acos",		"ArcCos",
	"atan", 	"ArcTan",
	"asinh", 	"ArcSinh", 
	"acosh", 	"ArcCosh", 
	"atanh", 	"ArcTanh",
	
	"acsc",		"ArcCsc",
	"acsch",	"ArcCsch",
	
	"asec",		"ArcSec",
	"asech",	"ArcSech",
	
	"acot",		"ArcCot",
	"acoth",	"ArcCoth",
	
	"exp", 		"Exp",
	"log", 		"Ln",
	"sqrt", 	"Sqrt",
	"choose", 	"Bin",
	"gamma", 	"Gamma",
	
	"!",		"Not",
	"==",		"=",
	"==",		"=",
	">=",		">=",
	">", 		">",
	"<=", 		"<=",
	"<", 		"<",
	"!=", 		"!=",
	":", 		"sequence",
	":", 		"..",
	
	"factorial","factorial",
	"factorial","!",
	"limit", 	"Limit",
	"deriv", 	"Deriv",
	"integrate","Integrate",
	"?",		"Taylor",

	"list",		"List",
	"TRUE",		"True",
	"<-",		":=",
	"Expr",		"",
	"Exprq", 	"",
	"expression",""
    
), byrow = TRUE, ncol = 2)
colnames(transtab) <- c("R", "yacas")

# Used for expressions not handled by R

root <- function(x, y) {
	(x)^(1/(y))
}






