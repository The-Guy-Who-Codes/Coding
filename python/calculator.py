memory = [0]

def addition(a, b):
  x = a + b
  return x

def subtraction(a, b):
  x  = a - b
  return x
  
def times(a, b):
  x = a * b
  return x
  
def division(a, b):
  x = a / b
  return x

while True:
  a = input("what is your first number: ")
  b = input("what is your second number: ")
  operator = input("what operation do you want to complete: ")
  
  if a == "ANS":
    a = memory[0]

  elif b == "ANS":
    b = memory[0]

  a = int(a)
  b = int(b)

  
  if operator == "+":
    ans = addition(a, b)
  
  elif operator == "-":
    ans = subtraction(a, b)
  
  elif operator == "*":
    ans = times(a, b)
  
  else:
    ans = division(a, b)
    
  print("the answer to your sum is: {}".format(ans))
  memory.pop()
  memory.append(ans)