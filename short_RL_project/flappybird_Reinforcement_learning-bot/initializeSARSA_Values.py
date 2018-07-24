import json
Q = {}

# write to json file
writeFile = open('SARSA_Learning.json', 'w')

for x in list(range(-40, 440, 20)):
    for y in list(range( -300, 435, 15)):
        for v in list(range(-10,11)):
            tup = (x, y, v)
            key = str(tup)
            Q[key] = [0, 0]


json.dump(Q, writeFile)            
writeFile.close()