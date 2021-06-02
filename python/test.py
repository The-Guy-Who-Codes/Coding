import math
x = 0
sum1 = 12 * ((((-1)**x)*math.factorial(6 * x)*(13591409+(545140134*x)))/(math.factorial(3*x)*(math.factorial(x)**3)*(640320)**((3*x)+(3/2)))) # = 1989 chudnovsky brothers formula

pi = 1/sum1
print(pi - math.pi)