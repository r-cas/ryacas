
## create a gui which asks for an R expression in x.  The code sets 
## the expression to zero and solves for x showing the result.

library(Ryacas)
library(tcltk)

fun <- function(instring) {
  s <- sprintf("Solve(%s, x)", instring)
  e <- as.expression(parse(text = s))
  yout <- yacas(e)$text[[1]][[2]]
  deparse(yout)
}

top <- tktoplevel()
output <- tclVar("")
input <- tclVar('x*x+x*y')

handler <- function() {
  tclvalue(output) <- fun(tclvalue(input))
}

lab1 <- tklabel(top, text='expression in x:')
entry1 <- tkentry(top, textvariable = input)
but2 <- tkbutton(top, text = 'derivate', command = handler)
lab2 <- tklabel(top, textvariable = output)

tkgrid(lab1, entry1)
tkgrid(but2, lab2)

