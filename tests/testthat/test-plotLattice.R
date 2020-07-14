# test-plotLattice.R
context("Testing plotLattice()")


lattice <- hip.lattice.polygon(box = matrix(c(0,0,1,1),2), J = c(2,2), bases = c(2,3) )


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("plotLattice(lattice) returns equivalent obj as it did previously", {
  expect_known_value(plotLattice(lattice), "test-plotLattice.rds")
})