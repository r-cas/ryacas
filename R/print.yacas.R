#' @export
print.yacas <- function(x, ...) {
	catn <- function(...) cat(..., "\n", sep = "")
	display <- list(...)$display
	if (is.null(display)) display <-
		if (!is.null(x$PrettyForm)) "PrettyForm"
		else if (!is.null(x$YacasForm)) "YacasForm"
	  else if (!is.null(x$LinAlgForm)) "LinAlgForm"
		else if (!is.null(x[[1]])) "expression"
		else if (!is.null(x$OMForm)) "OMForm"
	
	if (!is.null(display) && 
	    display == "expression" && 
	    RYACAS_OPTIONS("prettyform_default")) {
	  
	  x <- yacas(PrettyForm(x))
	  display <- "PrettyForm"
	}
	
	switch(display, 
		PrettyForm = for(s in x$PrettyForm) catn(s),
		YacasForm = catn(x$YacasForm),
		LinAlgForm = switch(x$LinAlgType,
		                    Vector = {
		                      catn("Yacas vector:")
		                      #catn(paste0(paste("(", x$LinAlgForm, ")"), collapse = ", "))
		                      #catn(paste0(paste("", x$LinAlgForm, ""), collapse = ", "))
		                      print(x$LinAlgForm, quote = FALSE)
		                    }, 
		                    Matrix = {
		                      catn("Yacas matrix:")
		                      print(x$LinAlgForm, quote = FALSE)
		                    }
		),
		expression = if (inherits(x[[1]], "character"))
				for(s in x[[1]]) catn(s)
			     else {
				print(x[[1]])
			     },
		OMForm = for(s in x$OMForm) catn(s))
	invisible()
}
