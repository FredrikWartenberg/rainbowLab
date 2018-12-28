## Test main function to generate rainbows
td <- generateRainbows(c(1,2),nColors=2, resolution=10,plot=FALSE)


## Test
test_that("Rainbow Generation", {
  expect_equal(round(sum(td$angD),4), 68.3191)
  expect_equal(attr(td,"intensityMethod"), "fresnel")
  expect_equal(attr(td,"resolution"), 10)
  expect_equal(attr(td,"nColors"), 2)
})