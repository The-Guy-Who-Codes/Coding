import random
import time

class slot():
    
    def Rand():
        global string
        string = []
        for x in range(0,5):
            string.append(random.randint(1,9))
        print(string)
    
    def jackpot():
        global jackpot
        jackpot = random.randint(75, 500)
        print("The Jackpot is: {}".format(jackpot))

z = True
bank = int(input("How much money do you want to deposit: "))

while z == True:
    slot.jackpot
    amount = int(input ("how many times do you want to spin(it costs 5 for every spin over 1): "))
    bank = bank - ((amount - 1) * 5)
    num = int(input("what number do you bet is the middle number on the screen: "))
    bet = int(input("what is the amount that you bet for this number: "))
    
    for x in range(0,amount):
        for x in range(0,10):
            time.sleep(0.5)
            slot.Rand()
        time.sleep(1.5)
            
        if string[0] == string[4] and string[0] == string[2]:
            print("You win the full jackpot")
            bank = bank + jackpot
            print("your balance is now: {}".format(bank))
            status = "win"
        
        elif string[0] == string[4]:
            print("you win half the jackpot")
            bank = bank + (jackpot / 2)
            print("your balance is now: {}".format(bank))
            status = "win"
        
        if string[2] == num:
            print("WOW you WIN!!!")
            bank = bank + (bet * 1.8)
            print("your bank balance is now: {}".format(bank))
            status = "win"
        else:
            print("O NO you didn't win this time")
            status = "lose"
            
        if status == "win":
            break

    if status != "win":   
        bank = bank - bet
        print("your bank balance is now: {}".format(bank))
    ask = input("do you want to play again?(y/n): ")
    
    if ask != "y":
        z = False

