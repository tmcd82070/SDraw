error.message<-function(message){
    dialog <- gtkMessageDialogNew(NULL, c("modal"), "error", "ok", message)
    dialog$run()
    dialog$destroy()
}
