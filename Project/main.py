import numpy as np
import scipy
from scipy import integrate
from scipy.signal import argrelextrema
import matplotlib.pyplot as plt


def wellPotential(x, width, value):
    return value if abs(x) > width else 0


def Schroed(y, r, V, E):
    psi, phi = y
    dphidx = [phi, (V - E) * psi]
    return np.asarray(dphidx)


def rk4(f, psi0, x, V, E):
    n = len(x)
    psi = np.array([psi0] * n)
    for i in range(n - 1):
        h = x[i + 1] - x[i]
        k1 = h * f(psi[i], x[i], V[i], E)
        k2 = h * f(psi[i] + 0.5 * k1, x[i] + 0.5 * h, V[i], E)
        k3 = h * f(psi[i] + 0.5 * k2, x[i] + 0.5 * h, V[i], E)
        k4 = h * f(psi[i] + k3, x[i + 1], V[i], E)
        psi[i + 1] = psi[i] + (k1 + 2.0 * (k2 + k3) + k4) / 6.0
    return psi


def findZeros(rightbound_vals):
    return np.where(np.diff(np.signbit(rightbound_vals)))[0]


def normalize(output_wavefunc):
    normal = max(abs(output_wavefunc))
    return output_wavefunc * (1 / (normal))


def RefineEnergy(Ebot, Etop, Nodes, psi0, x, V):
    tolerance = 1e-5
    ET = Etop
    EB = Ebot
    psi = [0]
    while abs(EB - ET) > tolerance or abs(psi[-1]) > 1e-5:
        initE = (ET + EB) / 2.0
        psi = rk4(Schroed, psi0, x, V, initE)[:, 0]
        nodes_ist = len(findZeros(psi)) - 1
        if nodes_ist > Nodes + 1:
            ET = initE
            continue
        if nodes_ist < Nodes - 1:
            EB = initE
            continue
        if nodes_ist % 2 == 0:
            if psi[len(psi) - 1] <= 0.0:
                ET = initE
            else:
                EB = initE
        elif nodes_ist > 0:
            if psi[len(psi) - 1] <= 0.0:
                EB = initE
            else:
                ET = initE
        elif nodes_ist < 0:
            continue
        last = abs(psi[-1])
    return EB, ET


def ShootingFinitePotentialWell(E_interval, nodes):
    psi_0 = 0.0
    phi_0 = 1.0
    psi_init = np.asarray([psi_0, phi_0])
    x_arr_ipw = np.arange(0, 3.0, 0.01)  # set up mesh
    V_ipw = [wellPotential(i, 0.5, 40) for i in x_arr_ipw]  # set up potential
    Eref, _ = RefineEnergy(E_interval[0], E_interval[1], nodes, psi_init,
                                x_arr_ipw, V_ipw)
    psi = rk4(Schroed, psi_init, x_arr_ipw, V_ipw, Eref)[:, 0]
    return Eref, normalize(psi), x_arr_ipw


E_fpw = [1., 400.0]
nodes_arr = np.arange(1, 3, 1)
print(" Welcome !")
print("Finite Potential Well Shooting ")
figfpw = plt.figure()
for ii in nodes_arr:
    Energy, psi_ipw, x_ipw = ShootingFinitePotentialWell(E_fpw, ii)
    print(" Found quantum state at energy = % s" % (Energy,))
    plt.cla()  # clear axis
    plt.clf()  # clear figure
    plt.plot(x_ipw, psi_ipw, 'b-.', label=r'$\ Psi (x) _{ num }$')
    plt.title('%s state  ' % (ii-1,))
    plt.legend(loc='best', fontsize='small')
    plt.grid()
    plt.show()

