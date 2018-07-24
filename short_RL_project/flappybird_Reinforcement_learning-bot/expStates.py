import math
x = 420
xList = []
yList = []
exp_rate = -0.25
while x >= 20:
    xList.append(x)
    xList.append(-x)
    yList.append(x)
    x = (int)(x*math.exp(exp_rate))

y = -320
while y <= -20:
    yList.append(y)
    y = (int)(y*math.exp(exp_rate))

xList.sort()
yList.sort()

print(xList)
print(yList)

for x in xList:
    for y in yList:
        for v in range(-10, 11):
            tup = (x, y, v)
            