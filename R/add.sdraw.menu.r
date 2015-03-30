add.sdraw.menu <- function(){
#
#   Add a menu to the R GUI under windows
#

sys <-  Sys.info()["sysname"]
if( sys == "Windows" & interactive() ){
    winMenuAdd("SDraw")
    winMenuAddItem("SDraw", "Equi-Probable Designs...", "equi.GUI()")
    #winMenuAddItem("SDraw", "Advanced Designs...", "advanced.GUI()")
} else {
    cat("This is a non-menu environment.\n")
    cat("Call one of the following functions:\n")
    cat("\t equi.GUI() -> Interface to draw equi-probable samples.\n")
    #cat("\t advanced.GUI() -> Interface to draw advanced samples.\n")
}

}
