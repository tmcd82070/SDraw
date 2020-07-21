# test-plotLattice.R
context("Testing plotLattice()")


lattice <- hip.lattice.polygon(box = matrix(c(0,0,1,1),2), J = c(2,2), bases = c(2,3) )


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("plotLattice(lattice) returns equivalent obj as it did previously", {
  expect_known_value(plotLattice(lattice), "plotLattice_case_1.rds")
})

# !is.null(J)
test_that("plotLattice(lattice, J = c(2,3)) returns equivalent obj as it did previously", {
  expect_known_value(plotLattice(lattice, J = c(2,3)), "plotLattice_case_2.rds")
})