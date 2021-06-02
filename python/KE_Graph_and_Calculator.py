import matplotlib.pyplot as plt

m = int(input("what is the mass of the object: "))
v = int(input("what is the velocity of the object: "))
Kinetic = 0.5 * m * (v ** 2)

print("the kinetic energy of your object is: " + str(Kinetic) + "J")

V_Graph = []
z = v

for x in range(0, 100):
    z += 1
    V_Graph.append(0.5 * m * (z ** 2))

M_Graph = []
a = m

for x in range(0, 100):
    a += 1
    M_Graph.append(0.5 *a * (v ** 2))
    
plt.plot(V_Graph)
plt.title("V += 1")
plt.xlabel("V == ")
plt.ylabel("Kinetic Energy")
plt.show()

plt.plot(M_Graph)
plt.title("M += 1")
plt.xlabel("M == ")
plt.ylabel("Kinetic Energy")
plt.show()
