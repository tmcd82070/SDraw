# test-plot.sdrawSample.R
context("Testing plotSample()")


# create spatial points object
# load pre-built dataset
data(meuse)
data(WY)

# prepare the coordinates
coords <- meuse[ , c("x", "y")]

# assign
spObj <- SpatialPoints(coords)

# create some arbitrary lines
l1 <- cbind(c(1, 2, 3), c(3, 2, 2))
l2 <- cbind(c(1, 2, 3), c(1, 1.5, 1))

# creat some formal class lines with these lines
Sl1 <- Line(l1)
Sl2 <- Line(l2)

# assign arbitrary ID's to these formal class lines
S1 <- Lines(list(Sl1), ID = "a")
S2 <- Lines(list(Sl2), ID = "b")

# create a formal class SpatialLines object
Sl <- SpatialLines(list(S1,S2))

# assign
HAL <- halton.lattice.polygon(WY, J=c(3,2), eta=c(3,2), triangular=TRUE )


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("plotSample(latticeTrue) returns equivalent obj as it did previously", {
  expect_known_value(plotSample(sdraw(HAL, 25, type="HIP"), WY, lattice=TRUE), "plotSample_latticeTrue.rds")
})

test_that("plotSample(missingFrame) returns equivalent obj as it did previously", {
  expect_known_value(plotSample(sdraw(HAL, 25, type="SRS")), "plotSample_missingFrame.rds")
})

test_that("sample type is SRS, poly.fill= FALSE", {
  expect_known_value(plotSample(sdraw(HAL, 10, type="SRS"), WY, lattice=TRUE, poly.fill= FALSE), "plotSample_polyfillFalse.rds")
})

test_that("x is SpatialLines", {
  expect_warning(
    expect_known_value(plotSample(sdraw(Sl, 25, type="BAS"),Sl , lattice=TRUE), "plotSample_SpatialLines.rds"))
})

test_that("x is SpatialPoints", {
  expect_known_value(plotSample(sdraw(spObj, 25, type="BAS"),spObj, lattice=TRUE ), "plotSample_SpatialPoints.rds")
})