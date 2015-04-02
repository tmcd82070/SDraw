add.sdraw.menu <- function(){
#
#   Add a menu to the R GUI under windows
#

sys <-  Sys.info()["sysname"]
rstudio <-  Sys.getenv("RSTUDIO") == "1"
if( sys == "Windows" & interactive() & !rstudio){
    winMenuAdd("SDraw")
	winMenuAddItem("SDraw/Sample Draws", "Equi-Probable...", "equi.GUI()")
	winMenuAddItem("SDraw/Sample Draws", "Stratified...", "stratified.GUI()")
	#winMenuAddItem("SDraw/Sample Draws", "Variable Probable...", "advanced.GUI()")
	winMenuAddItem("SDraw/Analysis", "CDF", "none")
} else if(!rstudio){
    cat("This is a non-menu environment.\n")
    cat("Call one of the following functions:\n")
    cat("\t equi.GUI() -> Interface to draw equi-probable samples.\n")
	cat("\t stratified.GIU() -> Interface to draw stratified samples.\n") #added by Guy, 1/2/15
    #cat("\t advanced.GUI() -> Interface to draw advanced samples.\n")
} else {
  winDialog( "ok", paste("You are running in RStudio where an SDraw menu cannot be created.", " ",
                         "Execute one of the following functions in the Console window:",
                         "  equi.GUI() -> Interface to draw equi-probable samples.",
                         "  stratified.GIU() -> Interface to draw stratified samples.", sep="\n"))
}

}
