OpenMath2R <- function(x) {
 out <- c()
 recurse <- function( x ) {
	if ("name" %in% names(xml2::xml_attrs(x))) {
		out <<- c(out, trans(xml2::xml_attr(x, "name"), from="OM", to="R"), " ")
	}
	if (xml2::xml_name(x) == "text") out <<- c(out, xml2::xml_text(x), " ")
	if (xml2::xml_name(x) == "OMF") out <<- c(out, xml2::xml_attr(x, "dec"), " ")
	if (xml2::xml_name(x) == "OMI") out <<- c(out, xml2::xml_text(x), " ")
	if (xml2::xml_name(x) == "OMS") {
	if (xml2::xml_attr(x, "cd") == "logic1" && "name" %in% names(xml2::xml_attrs(x))
		&& xml2::xml_attr(x, "name") %in% c("true", "false")) {}
	   else if ((xml2::xml_attr(x, "cd") != "nums1") ||
				(xml2::xml_attr(x, "name") == "rational")) 
			out <<- c(out, xml2::xml_text(x), "(")
	}
	# if (xml2::xml_name(x) == "OMS") out <<- c(out, "(")
	if (xml2::xml_name(x) == "OMSTR") {
	# out <<- c(out, sQuote(gsub("'", "\\\\'", xml2::xml_text(x))))
	out <<- c(out, paste("'", gsub("'", "\\\\'", xml2::xml_text(x)), "'", sep=""))
	} else if ( length( xml2::xml_children(x) ) > 0 )
		for( i in seq( along = xml2::xml_children(x) ) ) {
			Recall( xml2::xml_children(x)[[i]] )
			if (i > 1 && i < length(xml2::xml_children(x)))
				out <<- c(out, ",")
		}
	# if (xml2::xml_name(x) == "OMA" || xml2::xml_name(x) == "OMBIND") out <<- c(out, xml2::xml_text(x), ")")
	if (xml2::xml_name(x) == "OMA" || xml2::xml_name(x) == "OMBIND") out <<- c(out, ")")
 }
 x <- paste(x, "\n", collapse = "")
 x <- xml2::read_xml(x)
 x <- xml2::xml_root(x)
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






