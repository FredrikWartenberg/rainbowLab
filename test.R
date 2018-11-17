source("ray.R")


## test anlge
x = c(1,0,0)
y = c(0,1,0)
z = c(0,0,1)

## Test Coord Sys
print(CM <- coordSystem(x,y))


v45 = c(1,1,0)
v45 = v45/norm(v45,type="2")
vm45 = c(1,-1,0)
vm45 = vm45/norm(vm45,type="2")

print("Test angle")
##print(angle(x,y,z,v45)*180/pi)
##print(angle(x,y,z,vm45)*180/pi)
print(angle(CM,v45)*180/pi)
print(angle(CM,vm45)*180/pi)
