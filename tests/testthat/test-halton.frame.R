# test-halton.frame.R
context("Testing halton.frame()")


# create spatial points object
# load pre-built dataset
data(meuse)

# prepare the 3 components: coordinates, data, and proj4string
coords <- meuse[ , c("x", "y")]   # coordinates
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# assign
spObj <- SpatialPoints(coords)

# make the spatial points data frame object
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = data, 
                               proj4string = crs)

# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("halton.frame(spdf) returns equivalent obj as it did previously", {
  expect_known_value(halton.frame(spdf), "halton.frame.rds")
})

test_that("halton.frame() operates appropriately", {
  # use pre-built dataframe, 'mtcars' being my favorite
  # check if error is thrown when incorrect index name is given
  expect_error(halton.frame(mtcars, "gears", "Lowest to Highest"), "gears column not found in data frame.")
  
  # check if error is thrown with invalid dataframe
  expect_error(halton.frame(550, "", ""))
  
  # make sure the frame output is correctly formatted
  expect_type(halton.frame(mtcars, "hp", "Lowest to Highest"), "list")
  expect_length((halton.frame(mtcars, "hp", "Lowest to Highest")$"Lowest to Highest"), 32)
  expect_s3_class(halton.frame(mtcars, "disp", "Ascending Order"), "data.frame")
})

test_that("x is a SpatialPointsDataFrame",{
  expect_type(halton.frame(spdf, "cadmium", "Lowest to Highest"), "S4")
  expect_is(halton.frame(spdf, "cadmium", "Lowest to Highest"), "SpatialPointsDataFrame")
})