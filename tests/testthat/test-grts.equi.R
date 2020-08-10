# test-grts.equi.R
context("Testing grts.equi()")


# load a pre-built dataset
# this object needs to be a global variable, hence assignment
# and generation outside of the main test function. 
data(meuse)

# assign the 3 components: coordinates, data, and proj4string
coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# assign a spatial points data frame object from the 3 generated arguments
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)


test_that("grts.equi() operates on two different inheritance classes: integers and SPDF objects", {
  # this function operates on two different inheritance classes; integers and SPDF objects 
  # begin by testing when over.n == 0
  capture.output(expect_warning(expect_type(grts.equi(spdf, 20, 0), "S4")))
  capture.output(expect_warning(expect_visible(grts.equi(spdf, 20, 0), c("sampleID"))))
})

test_that("output formatted correctly", {
  # check that the SPDF object is formatted correctly when passing through this function
  capture.output(
    expect_warning(
      expect_named(grts.equi(spdf, 15, 3), c('sampleID',
                                             'pointType',
                                             'geometryID',
                                             'cadmium',
                                             'copper',
                                             'lead',
                                             'zinc',
                                             'elev',
                                             'dist',
                                             'om',
                                             'ffreq',
                                             'soil',
                                             'lime',
                                             'landuse',
                                             'dist.m'))))
  capture.output(expect_warning(expect_identical((grts.equi(spdf, 1, 0)$sampleID), "Site-1")))
  capture.output(expect_warning(expect_identical((grts.equi(spdf, 1, 0)$pointType), "Sample")))
})