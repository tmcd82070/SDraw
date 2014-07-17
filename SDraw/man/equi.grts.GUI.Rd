\name{equi.grts.GUI}
\alias{equi.grts.GUI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select equi-probable GRTS sample}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
equi.grts.GUI()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    require(RGtk2)
    require(sp)
    require(spsurvey)
    win <- gtkWindowNew("toplevel")
    win$setBorderWidth(8)
    win$setTitle("GRTS - GUI")
    vbox1 <- gtkVBoxNew(FALSE, 8)
    vbox1$setBorderWidth(8)
    win$add(vbox1)
    hbox1 <- gtkHBoxNew(FALSE, 8)
    hbox1$setBorderWidth(8)
    vbox1$add(hbox1)
    req.frame <- gtkFrameNew("Required Inputs")
    hbox1$add(req.frame)
    req.vbox <- gtkVBoxNew(FALSE, 8)
    req.vbox$setBorderWidth(8)
    req.frame$add(req.vbox)
    tbl <- gtkTable(3, 2, FALSE)
    gtkTableSetRowSpacings(tbl, 1)
    gtkTableSetColSpacings(tbl, 5)
    gtkBoxPackStart(req.vbox, tbl)
    n.entry <- gtkEntry()
    n.entry$setText("")
    samp.size.label <- gtkLabel("Sample size (n):")
    gtkTableAttach(tbl, samp.size.label, 0, 1, 0, 1, xpadding = 5, 
        ypadding = 5)
    gtkTableAttach(tbl, n.entry, 1, 2, 0, 1, xpadding = 5, ypadding = 5)
    shape.in.entry <- gtkEntry()
    shape.in.entry$setText("")
    shape.file.label <- gtkLabel("Shape file:")
    shape.file.box <- gtkHBox(FALSE, 10)
    browse.b <- gtkButton("Browse")
    gtkAddCallback(browse.b, "clicked", function(x) {
        browse.for.shapefile()
    })
    gtkBoxPackEnd(shape.file.box, browse.b)
    gtkBoxPackStart(shape.file.box, shape.in.entry)
    gtkTableAttach(tbl, shape.file.label, 0, 1, 1, 2, xpadding = 5, 
        ypadding = 5)
    gtkTableAttach(tbl, shape.file.box, 1, 2, 1, 2, xpadding = 5, 
        ypadding = 5)
    out.r.entry <- gtkEntry()
    out.r.entry$setText("grts.sample")
    out.r.label <- gtkLabel("Output R object:")
    gtkTableAttach(tbl, out.r.label, 0, 1, 2, 3, xpadding = 5, 
        ypadding = 5)
    gtkTableAttach(tbl, out.r.entry, 1, 2, 2, 3, xpadding = 5, 
        ypadding = 5)
    opt.frame <- gtkFrameNew("Optional Inputs")
    hbox1$add(opt.frame)
    opt.vbox <- gtkVBoxNew(FALSE, 8)
    opt.vbox$setBorderWidth(8)
    opt.frame$add(opt.vbox)
    opt.tbl <- gtkTable(3, 2, FALSE)
    gtkTableSetRowSpacings(tbl, 1)
    gtkTableSetColSpacings(tbl, 5)
    opt.vbox$add(opt.tbl)
    over.entry <- gtkEntry()
    over.entry$setText("0")
    over.size.label <- gtkLabel("Over sample:")
    gtkTableAttach(opt.tbl, over.size.label, 0, 1, 0, 1, xpadding = 5, 
        ypadding = 5)
    gtkTableAttach(opt.tbl, over.entry, 1, 2, 0, 1, xpadding = 5, 
        ypadding = 5)
    seed.entry <- gtkEntry()
    seed.entry$setText("")
    seed.label <- gtkLabel("Random number seed:")
    gtkTableAttach(opt.tbl, seed.label, 0, 1, 1, 2, xpadding = 5, 
        ypadding = 5)
    gtkTableAttach(opt.tbl, seed.entry, 1, 2, 1, 2, xpadding = 5, 
        ypadding = 5)
    samp.type.frame <- gtkFrameNew("Sample Type")
    hbox1$add(samp.type.frame)
    stype.box <- gtkVBoxNew(TRUE, 2)
    samp.type.frame$add(stype.box)
    area.rb <- gtkRadioButton(label = "Area\n(polygon shapefile)")
    line.rb <- gtkRadioButtonNewWithLabelFromWidget(area.rb, 
        "Linear\n(line shapefile)")
    disc.rb <- gtkRadioButtonNewWithLabelFromWidget(area.rb, 
        "Finite\n(point shapefile)")
    stype.box$packStart(area.rb, TRUE, TRUE, 2)
    stype.box$packStart(line.rb, TRUE, TRUE, 2)
    stype.box$packStart(disc.rb, TRUE, TRUE, 2)
    gtkBoxPackStart(vbox1, gtkHSeparatorNew(), expand = FALSE)
    bbox <- gtkHButtonBox()
    gtkHButtonBoxSetSpacingDefault(10)
    bbox$SetLayout("Spread")
    bbox$SetBorderWidth(10)
    run.b <- gtkButton("Run")
    gtkObjectAddCallback(run.b, "clicked", run.grts)
    gtkBoxPackEnd(bbox, run.b, expand = FALSE)
    plot.b <- gtkButton("Plot")
    gtkObjectAddCallback(plot.b, "clicked", plot.grts)
    gtkBoxPackEnd(bbox, plot.b, expand = FALSE)
    view.b <- gtkButton("View Sample")
    gtkObjectAddCallback(view.b, "clicked", view.grts)
    gtkBoxPackEnd(bbox, view.b, expand = FALSE)
    write.csv.b <- gtkButton("Write CSV")
    gtkObjectAddCallback(write.csv.b, "clicked", my.write.csv)
    gtkBoxPackEnd(bbox, write.csv.b, expand = FALSE)
    write.shp.b <- gtkButton("Write Shapefile")
    gtkObjectAddCallback(write.shp.b, "clicked", my.write.shp)
    gtkBoxPackEnd(bbox, write.shp.b, expand = FALSE)
    cancel.b <- gtkButton("Done")
    gtkAddCallback(cancel.b, "clicked", cleanup)
    gtkBoxPackEnd(bbox, cancel.b, expand = FALSE)
    gtkBoxPackEnd(vbox1, bbox, expand = FALSE)
    win$Show()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
