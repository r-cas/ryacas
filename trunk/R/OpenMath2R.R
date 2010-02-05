
OpenMath2R <- function(x) {
 out <- c()
 recurse <- function( x ) {
	if ("name" %in% names(xmlAttrs(x))) {
		out <<- c(out, trans(xmlAttrs(x)[["name"]], from="OM", to="R"), " ")
	}
	if (xmlName(x) == "text") out <<- c(out, xmlValue(x), " ")
	if (xmlName(x) == "OMF") out <<- c(out, xmlAttrs(x)[["dec"]], " ")
	if (xmlName(x) == "OMS") {
	if (xmlAttrs(x)[["cd"]] == "logic1" && "name" %in% names(xmlAttrs(x))
		&& xmlAttrs(x)[["name"]] %in% c("true", "false")) {}
	   else if ((xmlAttrs(x)[["cd"]] != "nums1") ||
				(xmlAttrs(x)[["name"]] == "rational")) 
			out <<- c(out, xmlValue(x), "(")
	}
	# if (xmlName(x) == "OMS") out <<- c(out, "(")
	if (xmlName(x) == "OMSTR") {
	# out <<- c(out, sQuote(gsub("'", "\\\\'", xmlValue(x))))
	out <<- c(out, paste("'", gsub("'", "\\\\'", xmlValue(x)), "'", sep=""))
	} else if ( length( xmlChildren(x) ) > 0 )
		for( i in seq( along = xmlChildren(x) ) ) {
			Recall( x[[i]] )
			if (i > 1 && i < length(xmlChildren(x))) 
				out <<- c(out, ",")
		}
	# if (xmlName(x) == "OMA" || xmlName(x) == "OMBIND") out <<- c(out, xmlValue(x), ")")
	if (xmlName(x) == "OMA" || xmlName(x) == "OMBIND") out <<- c(out, ")")
 }
 x <- paste(x, "\n", collapse = "")
 x <- xmlTreeParse(x, asText = TRUE)
 x <- xmlRoot(x)
 recurse(x)
 paste(out, collapse = "")
}

trans <- function(x, ttab=transtab, from, to) {
   idx <- match(x, ttab[,from], nomatch = 0)
   res <- if (idx > 0) ttab[idx,to] else x
   if (tolower(substr(res, 1, 1)) %in% letters) res
   else paste('"', res, '"', sep="")
}

transtab <- matrix( c(
	#R			OM			yacas
	"pi",		"pi",		"Pi",
	
	"+",		"plus",		"+",
	"-",		"minus",	"-",
	"*",		"times",	"*",
	"/",		"divide",	"/",
	"/",		"rational",	"/",
	"^",		"power",	"^",
	"%%",		"mod",		"Mod",
	"%/%",		"div",		"Div",
	"root",		"root",		"NthRoot",
	"Inf",		"infinity",	"Infinite",
	"NaN",		"undefined","Undefined",
	
	"sin",		"Sin",		"Sin",
	"cos",		"Cos",		"Cos",
	"tan",		"Tan",		"Tan",
	
	"asin",		"arcsin",	"ArcSin",
	"acos",		"arccos",	"ArcCos",
	"atan", 	"arctan", 	"ArcTan",
	"asinh", 	"arcsinh", 	"ArcSinh", 
	"acosh", 	"arccosh", 	"ArcCosh", 
	"atanh", 	"arctanh", 	"ArcTanh",
	
	"acsc",		"arccsc",	"ArcCsc",
	"acsch",	"arccsch",	"ArcCsch",
	
	"asec",		"arcsec",	"ArcSec",
	"asech",	"arcsech",	"ArcSech",
	
	"acot",		"arccot",	"ArcCot",
	"acoth",	"arccoth",	"ArcCoth",
	
	"exp", 		"exp", 		"Exp",
	"log", 		"ln", 		"Ln",
	"sqrt", 	"sqrt", 	"Sqrt",
	"choose", 	"bin", 		"Bin",
	"gamma", 	"gamma", 	"Gamma",
	
	"!",		"not",		"Not",
	"==",		"eq",		"=",
	"==",		"equivalent","=",
	">=",		"geq",		">=",
	">", 		"gt",		">",
	"<=", 		"leq",		"<=",
	"<", 		"lt",		"<",
	"!=", 		"neq",		"!=",
	":", 		"seq",		"sequence",
	":", 		"seq",		"..",
	
	"factorial","factorial","factorial",
	"factorial","factorial","!",
	"limit", 	"lim", 		"Limit",
	"deriv", 	"deriv", 	"Deriv",
	"integrate","integrate","Integrate",
	"?",		"taylor",	"Taylor",

	"list",		"List", 	"List",
	"TRUE",		"true", 	"True",
	"<-",		"?",		":=",
	"Expr",		"?",		"",
	"Exprq", 	"?",		"",
	"expression", 	"?", 		""
	
), byrow = TRUE, ncol = 3)
colnames(transtab) <- c("R", "OM", "yacas")

# Used for expressions not handled by R

root <- function(x, y) {
	(x)^(1/(y))
}






