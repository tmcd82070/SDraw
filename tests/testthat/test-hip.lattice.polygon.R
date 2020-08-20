# test-hip.lattice.polygon.R
context("Testing hip.lattice.polygon()")


# create spatial polygons object
v =  sample(1:10,24,replace = TRUE)
dim(v) = c(3,2,4)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("hip.lattice.polygon returns equivalent obj as it did previously", {
  expect_known_value(hip.lattice.polygon(box = matrix(data = c(0,1,0,1), nrow = 2, byrow = TRUE),
                                          J = c(3,2),
                                          bases = c(2,3)), "hip.lattice.polygon.rds")
})

test_that("output length is 108", {
  expect_length(hip.lattice.polygon(box = matrix(data = c(0,1,0,1), nrow = 2, byrow = TRUE),
                                   J = c(2,3),
                                   bases = c(2,3)), 108)
})

# check if the function stops with message
test_that("dimension is greater than 2", {
  expect_error(hip.lattice.polygon(box = v, getJ(72, bases = c(2,3)), bases = c(2,3)), "incorrect number of dimensions")
})