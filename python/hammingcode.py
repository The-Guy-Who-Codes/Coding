import math
import random

global size
size = 16
location = 13

def prnStorage(value):
    iteration = 0
    for x in range(int(math.log2(size))):
        pn = []
        for num in range(int(math.log2(size))):
            pn.append(value[iteration])
            iteration += 1
        print(pn)
    

def has1(array, iteration):
    times = 0
    numbers = list(array[iteration][1])
    for x in range(int(math.log2(size))):
        if numbers[x] == "1":
            times += 1
    if times == 1:
        return True
    else:
        return False
    
def has1where(array, where):
    parity = []
    for x in range(size):
        numbers = list(array[x][1])
        if numbers[where] == "1":
            parity.append(array[x])
    return parity
    
def fullbit(lst):  
    y = len(lst)
    tempString = ""
    for num in range(int(math.log2(size)) - y):
        tempString = "0" + tempString
    lst = tempString + lst
    return lst

def paritybit0(array):
    times = 0
    paritylst = list(array)
    for x in range(1, len(paritylst)):
        if paritylst[x][0] == 1:
            times += 1
    if times % 2 != 0:
        temp = array.index(paritylst[0])
        array[temp][0] = 1
    else:
        temp = array.index(paritylst[0])
        array[temp][0] = 0

def paritybit(array, where):
    times = 0
    paritylst = has1where(array, where)
    for x in range(1, len(paritylst)):
        if paritylst[x][0] == 1:
            times += 1
    if times % 2 != 0:
        temp = array.index(paritylst[0])
        array[temp][0] = 1
    else:
        temp = array.index(paritylst[0])
        array[temp][0] = 0

def paritychecker(array):
    print("\n")
    tarray = []
    for x in range(size):
        if array[x][0] == 1:
            tarray.append(array[x])
   # print(tarray)
    prevactionlst = list(tarray[0][1])
   # print(prevactionlst)
    for y in range(1, len(tarray)):
        nbit = ""
        actionlst = list(tarray[y][1])
       # print(actionlst)
        for z in range(len(actionlst)):
           # print(actionlst[z] + ":)")
           # print(int(prevactionlst[z]))
            nbitval = int(actionlst[z]) ^ int(prevactionlst[z])
            nbit = nbit + str(nbitval)
        prevactionlst = list(nbit)
    print(nbit)



storage = []
for x in range(size):
    storage.append([random.randint(0,1), bin(x)])
    temp = storage[x][1]
    z = temp.split("b")
    temp = z[1]
    storage[x][1] = temp
    y = len(storage[x][1])
    tempString = ""
    for num in range(int(math.log2(size)) - y):
        tempString = "0" + tempString
    storage[x][1] = tempString + storage[x][1]

prnStorage(storage)
    
paritybit(storage, 3)
paritybit(storage, 2)
paritybit(storage, 1)
paritybit(storage, 0)
paritybit0(storage)
print("\n")

prnStorage(storage)

temp = storage[location][0]
storage[location][0] = 1 if temp != 1 else 0

print("\n")
prnStorage(storage)


paritychecker(storage)