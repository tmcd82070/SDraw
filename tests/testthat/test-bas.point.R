context("Test the bas.point function")

##Load pre-built dataset
data(meuse)

# prepare the 3 components: coordinates, data, and proj4string

coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# make the spatial points data frame object
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)

test_that("bas.point() operates appropriately", {
  
  expect_silent(testSamp<-bas.point(spdf,20))
  expect_is(testSamp, "SpatialPointsDataFrame")
  
  ##These tests look at some individual facets of the base point function
  ##This is to ensure that the baseline function has operated as it should
  expect_equal(bas.point(spdf,20)$sampleID, rep(1:20))
  expect_equal(ncol(bas.point(spdf,20)), 14)
  expect_equal(nrow(bas.point(spdf,22)), 22)

  ##Now to check for specific column names within this spdf function
  expect_true("zinc" %in% names(bas.point(spdf,20)))
  expect_true("lead" %in% names(bas.point(spdf,20)))
  ##There should be no empty headers, test this below
  expect_false(" " %in% names(bas.point(spdf, 20)))
  
  ##Check every single column name, ensure proper generation
  answer= names(bas.point(spdf,10))
  expect_identical(names(bas.point(spdf,10)), answer)
  
  ##Use grepl function to check for exact matches
  answer1 = grepl("ffreq",names(bas.point(spdf,10)))
  answer2 = grepl("landuse",names(bas.point(spdf,10)))
  
  expect_identical(grepl("ffreq",names(bas.point(spdf,10))), answer1)
  expect_identical(grepl("landuse",names(bas.point(spdf,10))), answer2)
  
  
  
  ##To ensure that this function will not accept anything other than a SLDF object
  
  expect_error(bas.point(8,50), "Must call bas.point with a SpatialPoints* object.", fixed=TRUE)
})


