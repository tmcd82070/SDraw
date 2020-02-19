context("Test the halton.frame function")


test_that("halton.frame() operates appropriately", {
  
  ##Use pre-built dataframe, 'mtcars' being my favorite
  ##Check if error is thrown when incorrect index name is given
  expect_error(halton.frame(mtcars, "gears", "Lowest to Highest"), "gears column not found in data frame.")
  
  ##Check if error is thrown with invalid dataframe
  expect_error(halton.frame(550, "", ""))
  
  ##Make sure the frame output is correctly formatted
  expect_type(halton.frame(mtcars, "hp", "Lowest to Highest"), "list")
  expect_length((halton.frame(mtcars, "hp", "Lowest to Highest")$"Lowest to Highest"), 32)
  expect_s3_class(halton.frame(mtcars, "disp", "Ascending Order"), "data.frame")
})