# test-halton.lattice.polygon.R
context("Testing halton.lattice.polygon()")

# load data
data(WY)

# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("halton.lattice.polygon() returns equivalent obj as it did previously", {
  expect_known_value(halton.lattice.polygon(WY, eta=c(3,2), triangular=TRUE), "halton.lattice.polygon.rds")
})