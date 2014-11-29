

.onAttach<-function(libname, pkgname){

    v <- packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Draws (vers ", v ,")", sep=""))  
#   packageStartupMessage("\nWEST Inc. (tmcdonald@west-inc.com)") 


#    cat("\nSee docs/GTK2_Runtime_install_instructions.pdf for help installing the GTK+\n")
#    cat("runtime library required for R vesion >= 2.11.0 on Windows machines.\n")

    add.sdraw.menu()
    
#    SDrawNamespaceEnv = asNamespace( "SDraw" )
#    SDrawPackageSpace <- as.environment( "package:SDraw" )
#    print(ls(env=SDrawNamespaceEnv, all=T)) 
#    print(ls(env=SDrawPackageSpace, all=T)) 

#    assign(".INPUT.DIR", getwd(), envir=.GlobalEnv )
#    print(ls(env=.GlobalEnv, all=T))

}
