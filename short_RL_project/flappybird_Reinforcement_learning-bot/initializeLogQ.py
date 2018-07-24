import math
import json

# initialize q values on a log based scale# x -> [-420 to 420] y -> [-300 to 420]
x = 420
xStates = []
yStates = []
Q = {}
exp_rate = -0.18

# add x and -x (symmetric) also put positive x into y
while x >= 20:
    xStates.append(x)
    xStates.append(-x)
    
    yStates.append(x)

    x = (int)(x*math.exp(exp_rate))

y = -300
while y <= -20:
    yStates.append(y)
    y = (int)(y*math.exp(exp_rate))

xStates.sort()
yStates.sort()

for x in xStates:
    for y in yStates:
        for v in range(-10, 11):
            tup = (x, y, v)
            key = str(tup)
            Q[key] = [0, 0]

writeFile = open('logQValues.json', 'w')
json.dump(Q, writeFile)           
writeFile.close()

print(xStates)
print(yStates)

print(len(xStates)*len(yStates)*20)