file = open("test.txt", "r")

codes = []

while True:
    x = file.readline()
    if x != "":
        codes.append(x.strip())
    else:
        break

for x in range(len(codes)):
    codes[x] = codes[x].split(" ")
    codes[x].remove("Code:")
    codes[x] = codes[x][0]


array = map(int, codes)
#map(hex, codes)


file.close()