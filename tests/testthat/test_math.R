## Test coord system and angels

## Make coord syss
x = c(1,0,0)
y = c(0,1,0)
z = c(0,0,1)
CM <- coordSystem(x,y)

## Make vectors
v45 = c(1,1,0)
v45 = v45/norm(v45,type="2")
vm45 = c(1,-1,0)
vm45 = vm45/norm(vm45,type="2")

## Test
test_that("Angles of vectors relative to coord system", {
  expect_equal(c(angle(CM,v45)*180/pi), 45)
  expect_equal(c(angle(CM,vm45)*180/pi), -45)
})
