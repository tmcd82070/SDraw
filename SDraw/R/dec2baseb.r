dec2baseb <-
function(x, b=2){
#   convert x to base b representation
#   x vector must all be integers.  Could check for this using next three lines.
#        xi <- as.integer(x)
#        if(any(is.na(xi) | ((x-xi)!=0)))
#                print(list(ERROR="x not integer", x=x))
        N <- length(x)
        xMax <- max(x)
        ndigits <- (floor(logb(xMax, base=b))+1)
        Base.b <- array(NA, dim=c(N, ndigits))
        for(i in 1:ndigits){#i <- 1
                Base.b[, ndigits-i+1] <- (x %% b)
                x <- (x %/% b)
        }
#        if(N ==1) Base.b[1, ] else Base.b
        Base.b
} 



