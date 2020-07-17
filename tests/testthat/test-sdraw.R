# test-sdraw.R
context("Testing sdraw()")


# create spatial points object
# load pre-built dataset
data(WY)

# assign
srs <- sdraw(WY, 50, type="SRS")


# # the first run always succeeds, but warns
# # subsequent runs will suceed only if the file is unchanged
# # this will fail the first time if the output changes
# test_that("sdraw(srs) returns equivalent obj as it did previously", {
#   expect_known_value(srs, "sdraw.rds")
# })