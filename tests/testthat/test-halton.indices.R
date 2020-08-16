# test-halton.indices.R
context("Testing halton.indices()")


# create spatial points object
# load pre-built dataset
data(meuse)
data(WA.cities)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]
data   <- meuse[ , 3:14]          # data
crs    <- CRS("+init=epsg:28992") # proj4string of coords

# assign
spObj <- SpatialPoints(coords)
bb <- bbox(WA.cities) + c(0,0,1,1)

# assign cases
# x is SpatialPoints, missing hl.bbox
case_1 <- halton.indices(spObj, J=c(3,2))

# x is a data frame, J is null, missing hlbox, use.CRT=TRUE
case_2 <- halton.indices(coords, use.CRT=TRUE)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
# x is SpatialPoints, missing hl.bbox
test_that("halton.indices(case_1) returns equivalent obj as it did previously", {
  expect_known_value(case_1, "halton.indices_case_1.rds")
})

# # x is DataFrame, missing hl.bbox
test_that("halton.indices(case_2) returns equivalent obj as it did previously", {
  expect_known_value(case_2, "halton.indices_case_2.rds")
})

