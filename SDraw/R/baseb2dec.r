baseb2dec <-
function(x, b=2){
#   convert x from base b representation to base 10 (decimal)
#   x is a matrix, n rows by c columns, where n is number of numbers to convert, and columns represent digits in base b representation.
#   digits in x must all be integers.  Could check for this using next three lines.
#        xi <- as.integer(x)
#        if(any(is.na(xi) | ((x-xi)!=0)))
#                print(list(ERROR="x not integer", x=x))
        N <- nrow(x)
        ndigits <- ncol(x)
        bases <- b^((ndigits-1):0)
        bases <- matrix( bases, nrow=N, ncol=length(bases), byrow=T )

        Base.10 <- x * bases
        Base.10 <- apply(Base.10, 1, sum )

        Base.10
}



