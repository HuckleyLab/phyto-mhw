# Modified from http://monkeysuncle.stanford.edu/?p=485
# Upper and lower are se (or sd)

error.bar <- function(x, y, upper, lower=upper, length=0.1, dir='y', ...){
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
	stop("vectors must be same length")
	if(dir=='y') arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
	if(dir=='x') arrows(x-lower,y, x+upper, angle=90, code=3, length=length, ...)
}

