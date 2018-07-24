# open qvalues.json and write in a better format

readfile = open('logQValues.json', 'r')
writefile = open('logprettyQValues.json', 'w')

line = readfile.readline()
count = 0

while line:
    for word in line:
        for char in word:
            # write to pretty file
            writefile.write(char)

            if char == "{":
                writefile.write('\n')

            if char == ":":
                writefile.write('\t')

            # end of key-value pair
            if char == ",":
                count += 1

                if count%4 == 0:
                    writefile.write("\n")

    line = readfile.readline()

readfile.close()
writefile.close()