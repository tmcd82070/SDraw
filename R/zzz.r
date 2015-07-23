

.onAttach<-function(libname, pkgname){

    v <- packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  
#   packageStartupMessage("\nWEST Inc. (tmcdonald@west-inc.com)") 



}
