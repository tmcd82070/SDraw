cleanup <-
function(x){
    .win$Hide();
    .win$Destroy()

#    if( exists(".INPUT.DIR" )) remove( ".INPUT.DIR", pos=.GlobalEnv )
#    if( exists(".win" )) remove( ".win", pos=.GlobalEnv )
#    if( exists(".n.entry" )) remove( ".n.entry", pos=.GlobalEnv )
#    if( exists(".out.r.entry" )) remove( ".out.r.entry", pos=.GlobalEnv )
#    if( exists(".over.entry" )) remove( ".over.entry", pos=.GlobalEnv )
#    if( exists(".shape.in.entry" )) remove( ".shape.in.entry", pos=.GlobalEnv )
#    if( exists(".seed.entry" )) remove( ".seed.entry", pos=.GlobalEnv )
#    if( exists(".area.rb" )) remove( ".area.rb", pos=.GlobalEnv )
#    if( exists(".line.rb" )) remove( ".line.rb", pos=.GlobalEnv )
#    if( exists(".samp.type.combo" )) remove( ".samp.type.combo", pos=.GlobalEnv )
#    if( exists(".spin.win" )) remove( ".spin.win", pos=.GlobalEnv )
    NULL
}
