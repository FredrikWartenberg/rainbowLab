## Test main function to generate rainbows
td <- generateRainbows(c(1,2),nColors=2, resolution=10,plot=FALSE)


## Test
test_that("Rainbow Generation", {
  expect_equal(round(sum(td$angD),4), 68.3191)
  expect_equal(attr(td,"intensityMethod"), "fresnel_m")
  expect_equal(attr(td,"resolution"), 10)
  expect_equal(attr(td,"nColors"), 2)
})


test_that("Fresenel Intensities", {
    tda <- aggregateData(td)
    expect_equal(round(sd(tda$I),4), 0.0461)
    expect_equal(round(mean(tda$I),4), 0.0312)
 })
