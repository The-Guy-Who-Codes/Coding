import random

StackSize = 500

number = {
  0:"2",
  1:"3",
  2:"4",
  3:"5",
  4:"6",
  5:"7",
  6:"8",
  7:"9",
  8:"10",
  9:"Jack",
  10:"Queen",
  11:"King",
  12:"Ace"
}

def card():
    x = random.randint(0,12)
    card1 = {
    "name":number[x],
    "value":x + 2
    }
    if x > 7 and x < 12:
      card1["value"] = 10
    if x == 12:
      card1["value"] = 11
    return card1

while True:
  BetSize = int(input("what is your bet size, you have {} in the bank: ".format(StackSize)))
  StackSize -= BetSize
  
  playerCard = card()
  playerCard2 = card()
  dealerCard = card()
  dealerCard2 = card()
  
  playerValue = playerCard["value"] + playerCard2["value"]
  dealerValue = dealerCard["value"] + dealerCard2["value"]
  print("your cards are {} and {}, your value is {}".format(playerCard["name"], playerCard2["name"], playerValue))
  print("the dealers cards are {} and {}, his value is {}".format(dealerCard["name"], dealerCard2["name"], dealerValue))
  
  if playerValue == 21:
    print("BlackJack woooooo!")
    StackSize += BetSize * 3
    print("you stack size is now {}".format(StackSize))
  elif 21 - playerValue < 21 - dealerValue:
    print("you win!!!")
    StackSize += BetSize * 2
    print("you stack size is now {}".format(StackSize))
  elif 21 - playerValue == 21 - dealerValue:
    print("no one wins")
    print("you stack size is now {}".format(StackSize))
  else:
    print("the dealer wins")
    print("your stack is {}".format(StackSize))