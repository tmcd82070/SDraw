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
  winMenuAddItem("SDraw/Sample Draws", "Unequal...", "unequal.GUI()")
	#winMenuAddItem("SDraw/Sample Draws", "Variable Probable...", "advanced.GUI()")
	winMenuAddItem("SDraw/Analysis", "CDF", "none")
} else if(!rstudio){
  cat("This is a non-menu environment, but provided RGtk2 can be loaded.\n")
  cat("you can still use the dialogs.  To use the dialogs, call one of the following:\n")
  cat("\t equi.GUI() -> Equi-probable samples.\n")
	cat("\t stratified.GUI() -> Stratified samples.\n") #added by Guy, 1/2/15
  cat("\t unequalprob.GUI() -> Unequal probability samples.\n")
  #cat("\t advanced.GUI() -> Interface to draw advanced samples.\n")
} else {
  cat("You are running in RStudio where an SDraw menu cannot be created.\n")
  cat("To use the dialogs, execute one of the following in the Console window:\n")
  cat("  equi.GUI() -> Equi-probable samples.\n")
  cat("  stratified.GUI() -> Stratified samples.\n")
  cat("  unequal.GUI() -> Unequal probability samples.\n")
}

}