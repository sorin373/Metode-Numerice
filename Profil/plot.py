import matplotlib.pyplot as plt

def citeste(fisier):
    x = []
    y = []

    with open(fisier, "r") as f:
        for line in f:
            vals = line.split()

            x.append(float(vals[0]))
            y.append(float(vals[1]))
    return x, y

x_extrados, y_extrados = citeste("extrados.out")
x_intrados, y_intrados = citeste("intrados.out")

plt.figure(figsize=(10, 6))

plt.plot(x_extrados, y_extrados, linewidth=5, label="Extrados")
plt.plot(x_intrados, y_intrados, linewidth=5, label="Intrados")

plt.xlabel("x")
plt.ylabel("y")
plt.title("Generare profil aerodinamic cu ajutorul interpolarii")
plt.grid(True)

plt.axis("equal")
plt.legend()
plt.tight_layout()

plt.savefig("profil_ord_2.png", dpi=300)
plt.show()