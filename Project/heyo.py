import numpy as np
import scipy
from scipy import integrate
from scipy.signal import argrelextrema
import matplotlib.pyplot as plt

BOX_HALF_WIDTH = 10.0
X_MAX = 30.0
PSI_MAX = 10.0
TOL = 1.e-8
MAX_TRIALS = 100
HBARC = 1973
M_EC2 = 0.511e6
foo = 2.0 * M_EC2 / (HBARC * HBARC)

grid = 1000
V0 = 2000
parity = -1
E = 1
dE = 0.01
dx = X_MAX / (grid - 1)


def V(x):
    if abs(x) < BOX_HALF_WIDTH:
        return 0.0
    else:
        return V0


def DpsiDx(pp):
    return pp


def DpsiprimeDx(p, x):
    return foo * (V(x) - E) * p


psi = np.zeros(grid)
dpsi = np.zeros(grid)

x_grid = np.linspace(0, X_MAX, grid)

trial = 1
lastE = 0.01
last = 0
while trial <= MAX_TRIALS and abs((E - lastE) / lastE) > TOL:

    if (parity == 1):
        dpsi[0] = 0.0
        psi[0] = 1.0
    else:
        dpsi[0] = 1.0
        psi[0] = 0.0
    x = 0.
    i = 0
    kappa = (foo * (V(X_MAX) - E)) ** (1 / 2)
    while i < grid - 1 and abs(psi[i]) < PSI_MAX:
        p = psi[i]
        pp = dpsi[i]

        k1p = dx * DpsiDx(pp)
        k1pp = dx * DpsiprimeDx(p, x)

        x += 0.5 * dx

        k2p = dx * DpsiDx(pp + 0.5 * k1pp)
        k2pp = dx * DpsiprimeDx(p + 0.5 * k1p, x)

        k3p = dx * DpsiDx(pp + 0.5 * k2pp)
        k3pp = dx * DpsiprimeDx(p + 0.5 * k2p, x)

        x += 0.5 * dx

        k4p = dx * DpsiDx(pp + k3pp)
        k4pp = dx * DpsiprimeDx(p + k3p, x)

        dpsi[i + 1] = dpsi[i] + (k1pp + 2.0 * k2pp + 2.0 * k3pp + k4pp) / 6.0
        psi[i + 1] = psi[i] + (k1p + 2.0 * k2p + 2.0 * k3p + k4p) / 6.0
        i += 1
    test = dpsi[i] + kappa * psi[i]
    test = test / abs(test)
    if trial == 1:
        last = test
        E += dE
    else:
        if test * last < 0:
            dE *= 0.5
            E -= dE
        else:
            lastE = E
            E += dE
            last = test
    trial += 1
if trial >= MAX_TRIALS:
    print(f"Search did not converge in {trial - 1} iterations!\n")
    print("Final energy was %lf\n", E)
else:
    print(f"Search converged in {trial - 1} iterations.\n", )
    print(f"E = {E}")
if parity < 0:
    iedge = int(BOX_HALF_WIDTH / dx)
    psimax = 0.0
    for j in range(iedge):
        if abs(psi[j]) > psimax:
            psimax = abs(psi[j])
    for j in range(i):
        psi[j] /= psimax
plt.plot(x_grid, psi)
plt.show()
