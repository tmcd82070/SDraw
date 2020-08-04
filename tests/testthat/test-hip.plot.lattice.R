# test-hip.plot.lattice.R
context("Testing hip.plot.lattice()")


# load pre-built dataset
data(WA.cities)

# create a HIP sample
samp <- hip.point(WA.cities, 50)


# the first run always succeeds, but warns
# subsequent runs will suceed only if the file is unchanged
# this will fail the first time if the output changes
test_that("hip.plot.lattice(WA, sample = samp) returns equivalent obj as it did previously", {
  expect_known_value(hip.plot.lattice(WA, sample = samp), "hip.plot.lattice.rds")
})