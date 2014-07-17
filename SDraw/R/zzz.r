.onAttach<-function(libname, pkgname){

    library( RGtk2 )
    library( sp )
    library( spsurvey )

    v <- packageVersion("SDraw") 

    packageStartupMessage( paste("SDraw - Sample Selection (vers ", v ,")", sep=""))  
	packageStartupMessage("\nWEST Inc. (tmcdonald@west-inc.com)") 


#    cat("\nSee docs/GTK2_Runtime_install_instructions.pdf for help installing the GTK+\n")
#    cat("runtime library required for R vesion >= 2.11.0 on Windows machines.\n")

    add.sdraw.menu()

}
