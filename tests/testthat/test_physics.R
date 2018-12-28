## Physics functions

## Fresenel
testFunction <- fresnelSpecific(n=1.33,dir="o2i")

test_that("Fresenel Function", {
  expect_equal(round(testFunction(pi/3)$R_m,4), 0.0591)
})

