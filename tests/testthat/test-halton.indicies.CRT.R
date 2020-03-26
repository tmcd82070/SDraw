df <- data.frame(x=(0:100)/101, y = 0.2)

context("Test the halton.indices.CRT function")

test_that("halton.indices.CRT() operates appropriately", {
  
  ##Check that error catch for J = exponents operates as it should
  expect_error(halton.indices.CRT(df, c(1, 2),
                                  "number of boxes in one or more dimensions is not an integer power of bases. Check n.boxes and b."))
  ##Make sure that n.boxes only accepts vector arguments
  expect_error(halton.indices.CRT(df, 16),
               "number of boxes in one or more dimensions is not an integer power of bases. Check n.boxes and b.")

  expect_length(halton.indices.CRT(df, c(16, 9)), 101)
  expect_type(halton.indices.CRT(df, c(16, 9)), "double")
  
  ##Verify that output structure is the same as what is given in the original example
  expect_identical(halton.indices.CRT(df, c(16,9))[1:7], c(48, 48, 48, 48, 48, 48, 48))
  expect_identical(halton.indices.CRT(df, c(16,9))[80:85], c(3, 3, 3, 3, 75, 75))
  
})
