add.sdraw.menu <- function(){
#
#   Add a menu to the R GUI under windows
#

sys <-  Sys.info()["sysname"]
if( sys == "Windows" & interactive() ){
    winMenuAdd("SDraw")
	winMenuAddItem("SDraw/Draw a Sample", "Equi-Probable Designs...", "equi.GUI()")
	winMenuAddItem("SDraw/Draw a Sample", "Stratified sampling designs...", "stratified.GUI()")
	winMenuAddItem("SDraw/Analyze data", "CDF", "dummy")
    #winMenuAddItem("SDraw", "Advanced Designs...", "advanced.GUI()")
} else {
    cat("This is a non-menu environment.\n")
    cat("Call one of the following functions:\n")
    cat("\t equi.GUI() -> Interface to draw equi-probable samples.\n")
	cat("\t stratified.GIU() -> Interface to draw stratified samples.\n") #added by Guy, 1/2/15
    #cat("\t advanced.GUI() -> Interface to draw advanced samples.\n")
}

}
