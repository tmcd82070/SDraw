# test-halton.lattice.R
context("Testing halton.lattice()")


# load dataset
data(WY)


test_that("returns equivalent obj as it did previously", {
  expect_known_value(halton.lattice(box = matrix(1:6, ncol = 2, dimnames = list(c("X","Y","Z"), c("A","B"))), J=c(2,2), triangular = TRUE), "halton.lattice.rds")
})

test_that("stop when nrow(box) == length(bases)", {
  expect_error(halton.lattice(box = matrix(c(0,0,1,1),2), J=c(2,2), bases = c(3,5,7)), 
               "Dimensions must equal length of bases. Make nrow(box) == length(bases)", fixed =TRUE) 
})

test_that("dimensions must equal length of Eta parameter", {
  expect_error(halton.lattice(box = matrix(c(0,0,1,1),2), J=c(2,2), eta = 3), 
                              "Dimensions must equal length of Eta parameter.", fixed =TRUE)
})

test_that("dimension of triangular grids is not equal to 2", {
  expect_warning(halton.lattice(box=matrix(c(0,0,0,1,1,1),3), triangular = TRUE), 
                 "Triangular grids for D!=2 not implemented. Rectangular grid produced.", fixed =TRUE)
})